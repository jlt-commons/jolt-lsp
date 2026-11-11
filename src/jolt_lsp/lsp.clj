(ns jolt-lsp.lsp
  "LSP over stdio. Read features, diagnostics, and rename over the same engine."
  (:require [clojure.string :as string]
            [jolt-lsp.api :as api]
            [jolt-lsp.cursor :as cursor]
            [jolt-lsp.db :as db]
            [jolt-lsp.edit.rename :as rename]
            [jolt-lsp.forms :as forms]
            [jolt-lsp.indent :as indent]
            [jolt-lsp.lsp.rpc :as rpc]
            [jolt-lsp.paths :as paths]
            [jolt-lsp.query :as query]
            [jolt-lsp.text :as text]
            [jolt-lsp.tree :as tree]))

(defonce state (atom {:root nil :db nil :docs {}}))

(defn- root []
  (:root @state))

(defn- db* []
  (:db @state))

(defn- source-of [path]
  (or (get (:docs @state) path)
      (try (slurp path) (catch Throwable _ ""))))

(defn- uri->path [uri]
  (paths/uri->path uri))

(defn- path->uri [path]
  (paths/file-uri path))

(defn- internal [source pos]
  (let [line (:line pos)
        ch (:character pos)
        row (inc (or line 0))]
    [row (inc (text/utf16->codepoint-col source row (or ch 0)))]))

(defn- lsp-range [source [r c er ec]]
  {:start {:line (dec r)
           :character (text/codepoint->utf16-col source r (dec c))}
   :end {:line (dec er)
         :character (text/codepoint->utf16-col source er (dec ec))}})

(defn- location->lsp [loc]
  (let [source (source-of (:path loc))]
    {:uri (path->uri (:path loc))
     :range (lsp-range source (:range loc))}))

(defn- publish-diagnostics [path]
  (let [source (source-of path)
        ds (api/diagnostics* (root) {:file path})]
    (rpc/notify "textDocument/publishDiagnostics"
                {:uri (path->uri path)
                 :diagnostics (mapv (fn [d]
                                      {:range (lsp-range source [(d :row) (d :col)
                                                                 (d :end-row) (d :end-col)])
                                       :severity (if (= :error (:severity d)) 1 2)
                                       :code (name (:code d))
                                       :source "jolt-lsp"
                                       :message (:message d)})
                                    ds)})))

(defn- refresh [root]
  (let [d (api/refresh! root)]
    (swap! state assoc :root root :db d)))

(defn- handle-definition [params]
  (let [path (uri->path (get-in params [:textDocument :uri]))
        source (source-of path)
        [row col] (internal source (:position params))
        d (db*)]
    (mapv location->lsp (query/definition-at d path row col))))

(defn- handle-references [params]
  (let [path (uri->path (get-in params [:textDocument :uri]))
        source (source-of path)
        [row col] (internal source (:position params))
        d (db*)
        locs (query/definition-at d path row col)
        fqn (:fqn (first locs))]
    (if fqn
      (mapv location->lsp (query/references d fqn))
      [])))

(defn- handle-hover [params]
  (let [path (uri->path (get-in params [:textDocument :uri]))
        source (source-of path)
        [row col] (internal source (:position params))
        d (db*)
        loc (first (query/definition-at d path row col))
        h (when loc (query/hover d (:fqn loc)))]
    (when h
      {:contents {:kind "markdown"
                  :value (str "```clojure\n" (:arglists h) "\n```\n\n"
                              (or (:doc h) ""))}
       :range (lsp-range source (:range h))})))

(defn- handle-document-symbols [params]
  (let [path (uri->path (get-in params [:textDocument :uri]))
        source (source-of path)
        d (db*)]
    (->> (query/document-symbols d path)
         (mapv (fn [s]
                 {:name (:name s)
                  :kind (case (:kind s)
                          (:defn :defmacro :defn-) 12
                          :def 13
                          :defprotocol 11
                          :protocol-method 6
                          :defrecord 23
                          :deftype 23
                          :defmulti 6
                          13)
                  :range (lsp-range source (:range s))
                  :selectionRange (lsp-range source (:selection-range s))})))))

(defn- handle-workspace-symbols [params]
  (let [q (get-in params [:query])
        d (db*)]
    (->> (query/search d q 100)
         (mapv (fn [s]
                 {:name (:name s)
                  :kind 13
                  :location {:uri (path->uri (:path s))
                             :range (lsp-range (source-of (:path s)) (:range s))}})))))

(defn- handle-rename [params]
  (let [path (uri->path (get-in params [:textDocument :uri]))
        source (source-of path)
        [row col] (internal source (:position params))
        to (:newName params)
        d (db*)
        r (rename/rename d {:path path :row row :col col :to to})]
    (if (:ok r)
      {:changes (into {}
                      (map (fn [[p edits]]
                             [(path->uri p)
                              (mapv (fn [e]
                                      {:range (lsp-range (source-of p)
                                                         [(:start-row e) (:start-col e)
                                                          (:end-row e) (:end-col e)])
                                       :newText (:new-text e)})
                                    edits)]))
                      (:edits r))}
      {:error (:reason r)})))

(defn- handle-selection [params]
  (let [path (uri->path (get-in params [:textDocument :uri]))
        source (source-of path)
        chain (fn [pos]
                (let [[row col] (internal source pos)
                      ns (cursor/enclosing-forms source row col)]
                  (reduce (fn [parent n]
                            {:range (lsp-range source (tree/node-range n))
                             :parent parent})
                          nil
                          (reverse ns))))]
    (mapv chain (:positions params))))

(defn- handle-completion [params]
  (let [path (uri->path (get-in params [:textDocument :uri]))
        source (source-of path)
        [row col] (internal source (:position params))
        line (nth (string/split-lines source) (dec row) "")
        prefix (or (last (re-find #"[^\s()\[\]{}]*"
                                   (subs line 0 (min (count line) (dec col)))))
                   "")
        d (db*)]
    {:isIncomplete false
     :items (->> (query/search d prefix 50)
                 (mapv (fn [s]
                         {:label (:name s)
                          :kind 3
                          :detail (:arglists s)
                          :documentation (:doc s)
                          :data {:fqn (:fqn s)}})))}))

(defn- handle-highlight [params]
  (let [path (uri->path (get-in params [:textDocument :uri]))
        source (source-of path)
        [row col] (internal source (:position params))
        d (db*)
        loc (first (query/definition-at d path row col))
        fqn (:fqn loc)]
    (if fqn
      (->> (query/references d fqn)
           (filter #(= path (:path %)))
           (mapv (fn [l] {:range (lsp-range source (:range l)) :kind 1})))
      [])))

(defn- handle-folding [params]
  (let [path (uri->path (get-in params [:textDocument :uri]))
        source (source-of path)]
    (->> (forms/top-level-forms source)
         (keep (fn [f]
                 (let [[r _ er _] (:range f)]
                   (when (< r er)
                     {:startLine (dec r) :endLine (dec er)}))))
         vec)))

(defn- handle-on-type [params]
  (let [path (uri->path (get-in params [:textDocument :uri]))
        source (source-of path)
        [row col] (internal source (:position params))
        ind (indent/indent source row col)]
    [{:range {:start (:position params) :end (:position params)}
      :newText ind}]))

(defn- handle-code-actions [params]
  (let [path (uri->path (get-in params [:textDocument :uri]))
        r (api/clean-ns* (root) {:file path})]
    (if (and (:ok r) (:changed r))
      (let [source (source-of path)
            edits (mapv (fn [e]
                          {:range (lsp-range source [(:start-row e) (:start-col e)
                                                     (:end-row e) (:end-col e)])
                           :newText (:new-text e)})
                        (:edits r))]
        [{:title "Clean namespace"
          :kind "source.organizeImports"
          :edit {:changes {(path->uri path) edits}}}])
      [])))

(defn- handle-signature [params]
  (let [path (uri->path (get-in params [:textDocument :uri]))
        source (source-of path)
        [row col] (internal source (:position params))
        d (db*)
        loc (first (query/definition-at d path row col))
        h (when loc (query/hover d (:fqn loc)))]
    (when (and h (:arglists h))
      {:signatures [{:label (:arglists h) :documentation (:doc h)}]
       :activeSignature 0
       :activeParameter 0})))

(defn- handle-prepare-rename [params]
  (let [path (uri->path (get-in params [:textDocument :uri]))
        source (source-of path)
        [row col] (internal source (:position params))
        tok (cursor/symbol-at source row col)]
    (when tok
      {:range (lsp-range source (:range tok))
       :placeholder (:text tok)})))

(def token-types ["namespace" "type" "function" "macro" "variable"
                  "keyword" "parameter"])

(defn- token-type-index [t]
  (case t
    :type 1
    :function 2
    :macro 3
    :variable 4
    :keyword 5
    :parameter 6
    4))

(defn- var-kind->token [kind]
  (case kind
    (:defn :defmethod :defmulti) :function
    :defmacro :macro
    (:defprotocol :defrecord :deftype) :type
    :variable))

(defn- semantic-token* [source row col er ec type]
  (when (and row col er ec (= row er))
    (let [start (text/codepoint->utf16-col source row (dec col))
          end (text/codepoint->utf16-col source er (dec ec))
          len (- end start)]
      (when (pos? len)
        [row start len (token-type-index type) 0]))))

(defn- semantic-token-name [source r type]
  (semantic-token* source (:name-row r) (:name-col r)
                   (:name-end-row r) (:name-end-col r) type))

(defn- semantic-token-body [source r type]
  (semantic-token* source (:row r) (:col r) (:end-row r) (:end-col r) type))

(defn- handle-semantic-tokens [params]
  (let [path (uri->path (get-in params [:textDocument :uri]))
        source (source-of path)
        file (db/file-by-path (db*) path)
        fid (:id file)
        d (db*)
        var-kinds (into {} (map (juxt :fqn :kind)) (db/all-var-defs d))
        toks (concat
              (keep #(semantic-token-name source % (var-kind->token (:kind %)))
                    (db/var-defs-in-file d fid))
              (keep #(semantic-token-name source %
                                     (var-kind->token (get var-kinds (:fqn %))))
                    (db/var-refs-in-file d fid))
              (keep #(semantic-token-body source % :keyword)
                    (db/keyword-defs-in-file d fid))
              (keep #(semantic-token-body source % :keyword)
                    (db/keyword-refs-in-file d fid))
              (keep #(semantic-token-body source % :parameter)
                    (db/local-defs-in-file d fid))
              (keep #(semantic-token-body source % :parameter)
                    (db/local-refs-in-file d fid)))
        sorted (sort-by (juxt first second) (filter some? toks))
        data (loop [rows sorted prev-line 0 prev-start 0 out []]
               (if (empty? rows)
                 out
                 (let [[line start len typ mods] (first rows)
                       dl (- line prev-line)
                       ds (if (zero? dl) (- start prev-start) start)]
                   (recur (rest rows) line start
                          (conj out dl ds len typ mods)))))]
    {:data data}))

(defn- handle-code-lens [params]
  (let [path (uri->path (get-in params [:textDocument :uri]))
        source (source-of path)
        d (db*)
        file (db/file-by-path d path)]
    (->> (db/var-defs-in-file d (:id file))
         (mapv (fn [v]
                 (let [n (count (db/refs-by-fqn d (:fqn v)))]
                   {:range (lsp-range source [(:name-row v) (:name-col v)
                                              (:name-end-row v) (:name-end-col v)])
                    :command {:title (str n (if (= n 1) " reference" " references"))
                              :command ""}}))))))

(defn- handle-formatting [params]
  (let [path (uri->path (get-in params [:textDocument :uri]))
        source (source-of path)
        r (jolt-lsp.format/format-string (root) source)]
    (if (and (:ok r) (not= source (:text r)))
      (let [lines (string/split source #"\n" -1)]
        [{:range {:start {:line 0 :character 0}
                  :end {:line (dec (count lines))
                        :character (count (last lines))}}
          :newText (:text r)}])
      [])))

(defn- handle [msg]
  (let [method (:method msg)
        id (:id msg)
        params (:params msg)]
    (case method
      "initialize"
      (let [uri (or (get-in params [:rootUri])
                    (get-in params [:workspaceFolders 0 :uri]))
            root (or (when uri (uri->path uri))
                     (System/getProperty "user.dir"))
            d (api/refresh! root)]
        (swap! state assoc :root root :db d)
        (rpc/respond id {:capabilities
                         {:textDocumentSync {:openClose true :change 1 :save true}
                          :definitionProvider true
                          :referencesProvider true
                          :hoverProvider true
                          :documentSymbolProvider true
                          :workspaceSymbolProvider true
                          :documentHighlightProvider true
                          :renameProvider {:prepareProvider false}
                          :selectionRangeProvider true
                          :foldingRangeProvider true
                          :codeActionProvider true
                          :onTypeFormattingProvider {:firstTriggerCharacter "\n"}
                          :signatureHelpProvider {:triggerCharacters [" " "("]}
                          :completionProvider {:resolveProvider false}
                          :semanticTokensProvider {:legend {:tokenTypes token-types
                                                            :tokenModifiers []}
                                                   :full true}
                          :codeLensProvider {:resolveProvider false}
                          :documentFormattingProvider true}
                         :serverInfo {:name "jolt-lsp" :version "0.1.0"}}))

      "initialized" nil

      "textDocument/didOpen"
      (let [path (uri->path (get-in params [:textDocument :uri]))
            text (get-in params [:textDocument :text])]
        (swap! state assoc-in [:docs path] text)
        (refresh (root))
        (publish-diagnostics path))

      "textDocument/didChange"
      (let [path (uri->path (get-in params [:textDocument :uri]))
            text (get-in params [:contentChanges 0 :text])]
        (swap! state assoc-in [:docs path] text)
        (api/refresh-file! (root) path text)
        (publish-diagnostics path))

      "textDocument/didSave"
      (let [path (uri->path (get-in params [:textDocument :uri]))]
        (api/refresh-file! (root) path)
        (publish-diagnostics path))

      "textDocument/didClose"
      (let [path (uri->path (get-in params [:textDocument :uri]))]
        (swap! state update :docs dissoc path))

      "textDocument/definition" (rpc/respond id (handle-definition params))
      "textDocument/references" (rpc/respond id (handle-references params))
      "textDocument/hover" (rpc/respond id (handle-hover params))
      "textDocument/documentSymbol" (rpc/respond id (handle-document-symbols params))
      "workspace/symbol" (rpc/respond id (handle-workspace-symbols params))
      "textDocument/rename" (rpc/respond id (handle-rename params))
      "textDocument/selectionRange" (rpc/respond id (handle-selection params))
      "textDocument/completion" (rpc/respond id (handle-completion params))
      "textDocument/documentHighlight" (rpc/respond id (handle-highlight params))
      "textDocument/foldingRange" (rpc/respond id (handle-folding params))
      "textDocument/onTypeFormatting" (rpc/respond id (handle-on-type params))
      "textDocument/codeAction" (rpc/respond id (handle-code-actions params))
      "textDocument/signatureHelp" (rpc/respond id (handle-signature params))
      "textDocument/prepareRename" (rpc/respond id (handle-prepare-rename params))
      "textDocument/semanticTokens/full" (rpc/respond id (handle-semantic-tokens params))
      "textDocument/codeLens" (rpc/respond id (handle-code-lens params))
      "textDocument/formatting" (rpc/respond id (handle-formatting params))
      "shutdown" (rpc/respond id nil)
      "exit" (System/exit 0)
      (when id (rpc/respond-error id -32601 (str "unknown method " method))))))

(defn -main [& _]
  (loop []
    (when-let [msg (rpc/read-message)]
      (try
        (handle msg)
        (catch Throwable e
          (when (:id msg)
            (rpc/respond-error (:id msg) -32603 (str e)))))
      (recur)))
  (System/exit 0))
