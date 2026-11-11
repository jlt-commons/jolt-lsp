(ns jolt-lsp.lsp
  "LSP over stdio. Read features, diagnostics, and rename over the same
  engine and the same index as the agent API (jolt-lsp.api/state).

  Open buffers are authoritative: their text is what is indexed and what
  positions are read against, saved or not."
  (:require [clojure.string :as string]
            [jolt-lsp.api :as api]
            [jolt-lsp.cursor :as cursor]
            [jolt-lsp.db :as db]
            [jolt-lsp.edit.clean-ns :as clean-ns]
            [jolt-lsp.edit.rename :as rename]
            [jolt-lsp.format :as fmt]
            [jolt-lsp.forms :as forms]
            [jolt-lsp.indent :as indent]
            [jolt-lsp.lsp.rpc :as rpc]
            [jolt-lsp.paths :as paths]
            [jolt-lsp.query :as query]
            [jolt-lsp.text :as text]
            [jolt-lsp.text.index :as tix]
            [jolt-lsp.tree :as tree]
            [jolt-lsp.version :as version]))

(defonce state (atom {:docs {}}))

(defn- root [] (:root @api/state))

(defn- db* [] (:db @api/state))

(defn- uri->path [uri]
  (some-> (paths/uri->path uri) db/normalize-path))

(defn- path->uri [path]
  (paths/file-uri path))

(defn- source-of [path]
  (or (get (:docs @state) path)
      (try (slurp path) (catch Throwable _ ""))))

(defn- doc-path [params]
  (uri->path (get-in params [:textDocument :uri])))

(defn- internal [source pos]
  (let [row (inc (or (:line pos) 0))]
    [row (inc (text/utf16->codepoint-col source row (or (:character pos) 0)))]))

(defn- lsp-range [source [r c er ec]]
  {:start {:line (dec r)
           :character (tix/codepoint->utf16-col source r (dec c))}
   :end {:line (dec er)
         :character (tix/codepoint->utf16-col source er (dec ec))}})

(defn- location->lsp [loc]
  {:uri (path->uri (:path loc))
   :range (lsp-range (source-of (:path loc)) (:range loc))})

(defn- at-cursor
  "[path source row col] for a textDocument/position request."
  [params]
  (let [path (doc-path params)
        source (source-of path)
        [row col] (internal source (:position params))]
    [path source row col]))

(defn- definitions [params]
  (let [[path source row col] (at-cursor params)]
    (query/definition-at (db*) path row col source)))

(defn- publish-diagnostics [path]
  (let [source (source-of path)
        ds (api/diagnostics* (root) {:file path :source source})]
    (rpc/notify "textDocument/publishDiagnostics"
                {:uri (path->uri path)
                 :diagnostics (mapv (fn [d]
                                      {:range (lsp-range source [(:row d) (:col d)
                                                                 (:end-row d) (:end-col d)])
                                       :severity (case (:severity d) :error 1 :warning 2 :info 3 2)
                                       :code (name (:code d))
                                       :source (name (or (:source d) :jolt-lsp))
                                       :message (:message d)})
                                    ds)})))

(defn- handle-definition [params]
  (mapv location->lsp (definitions params)))

(defn- handle-references [params]
  (let [fqn (:fqn (first (definitions params)))]
    (if fqn
      (mapv location->lsp (query/references (db*) fqn))
      [])))

(defn- handle-hover [params]
  (let [[_ source row col] (at-cursor params)
        loc (first (definitions params))
        h (when (:fqn loc) (query/hover (db*) (:fqn loc)))
        tok (query/token-at source row col)]
    (when h
      (cond-> {:contents {:kind "markdown"
                          :value (str "**" (:fqn h) "**\n\n"
                                      (when (seq (:arglists h))
                                        (str "```clojure\n" (:arglists h) "\n```\n\n"))
                                      (or (:doc h) ""))}}
        tok (assoc :range (lsp-range source (:range tok)))))))

(defn- symbol-kind [kind]
  (case kind
    (:defn :defmacro :deftest) 12
    :defprotocol 11
    :protocol-method 6
    (:defrecord :deftype) 23
    :defmulti 12
    13))

(defn- handle-document-symbols [params]
  (let [path (doc-path params)
        source (source-of path)]
    (->> (query/document-symbols (db*) path)
         (mapv (fn [s]
                 {:name (:name s)
                  :kind (symbol-kind (:kind s))
                  :range (lsp-range source (:range s))
                  :selectionRange (lsp-range source (:selection-range s))})))))

(defn- handle-workspace-symbols [params]
  (let [sources (atom {})
        src (fn [p] (or (get @sources p) (let [s (source-of p)] (swap! sources assoc p s) s)))]
    (->> (query/search (db*) (or (:query params) "") 100)
         (mapv (fn [s]
                 {:name (:name s)
                  :containerName (some-> (:fqn s) (string/split #"/") first)
                  :kind (symbol-kind (:kind s))
                  :location {:uri (path->uri (:path s))
                             :range (lsp-range (src (:path s)) (:range s))}})))))

(defn- rename-result [params]
  (let [[path _ row col] (at-cursor params)]
    (rename/rename (db*) {:path path :row row :col col :to (:newName params)
                          :sources (:docs @state)})))

(defn- handle-selection [params]
  (let [path (doc-path params)
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
  (let [[path source row col] (at-cursor params)
        line (nth (string/split source #"\n" -1) (dec row) "")
        prefix (or (re-find #"[^\s()\[\]{}\"'`~@^,;]*$" (subs line 0 (min (count line) (dec col))))
                   "")
        d (db*)
        slash (string/index-of prefix "/")
        alias (when slash (subs prefix 0 slash))
        target (when alias
                 (or (some #(when (= alias (:alias %)) (:target %)) (db/file-aliases d path))
                     alias))
        qprefix (if slash (subs prefix (inc slash)) prefix)
        items (if target
                (->> (db/vars-in-ns d target)
                     (remove :private)
                     (filter #(string/starts-with? (:name %) qprefix))
                     (sort-by :name)
                     (map #(assoc % :label (str alias "/" (:name %)))))
                (map #(assoc % :label (:name %)) (query/search d prefix 50)))]
    {:isIncomplete true
     :items (mapv (fn [s]
                    {:label (:label s)
                     :kind (case (:kind s) :defmacro 14 (:defn :defmulti :protocol-method) 3 6)
                     :detail (str (:fqn s) (when (seq (:arglists s)) (str " " (:arglists s))))
                     :documentation (:doc s)})
                  (take 100 items))}))

(defn- handle-highlight [params]
  (let [[path source] (at-cursor params)
        fqn (:fqn (first (definitions params)))]
    (if fqn
      (->> (query/references (db*) fqn)
           (filter #(= path (:path %)))
           (mapv (fn [l] {:range (lsp-range source (:range l)) :kind 1})))
      [])))

(defn- handle-folding [params]
  (let [source (source-of (doc-path params))]
    (->> (forms/top-level-forms source)
         (keep (fn [f]
                 (let [[r _ er _] (:range f)]
                   (when (< r er)
                     {:startLine (dec r) :endLine (dec er)}))))
         vec)))

(defn- handle-on-type [params]
  (let [[_ source row col] (at-cursor params)]
    [{:range {:start (:position params) :end (:position params)}
      :newText (indent/indent source row col)}]))

(defn- edits->lsp [source edits]
  (mapv (fn [e]
          {:range (lsp-range source [(:start-row e) (:start-col e)
                                     (:end-row e) (:end-col e)])
           :newText (:new-text e)})
        edits))

(defn- handle-code-actions [params]
  (let [path (doc-path params)
        source (source-of path)
        r (try (clean-ns/plan (db*) path source) (catch Throwable _ nil))]
    (if (and (:ok r) (:changed r))
      [{:title "Clean namespace"
        :kind "source.organizeImports"
        :edit {:changes {(path->uri path) (edits->lsp source (:edits r))}}}]
      [])))

(defn- handle-signature [params]
  (let [[_ source row col] (at-cursor params)
        ;; the head of the innermost call around the cursor
        call (some (fn [n]
                     (when (= :list (tree/tag n))
                       (let [h (first (tree/children n))]
                         (when (and h (tree/symbol-node? h)) [n h]))))
                   (reverse (cursor/enclosing-forms source row col)))
        [_ head] call
        [hr hc] (when head (tree/node-range head))
        loc (when head (first (query/definition-at (db*) (doc-path params) hr hc source)))
        h (when (:fqn loc) (query/hover (db*) (:fqn loc)))]
    (when (and h (seq (:arglists h)))
      {:signatures (mapv (fn [a] {:label (str "(" (:name h) " " (subs a 2))
                                  :documentation (:doc h)})
                         (re-seq #"\(\[[^\]]*\]\)" (:arglists h)))
       :activeSignature 0})))

(defn- handle-prepare-rename [params]
  (let [[_ source row col] (at-cursor params)
        tok (query/token-at source row col)]
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
    (:defn :defmethod :defmulti :protocol-method) :function
    :defmacro :macro
    (:defprotocol :defrecord :deftype) :type
    :variable))

(defn- semantic-token* [source row col er ec type]
  (when (and row col er ec (= row er))
    (let [start (tix/codepoint->utf16-col source row (dec col))
          end (tix/codepoint->utf16-col source er (dec ec))
          len (- end start)]
      (when (pos? len)
        ;; LSP lines are 0-based
        [(dec row) start len (token-type-index type) 0]))))

(defn- handle-semantic-tokens [params]
  (let [path (doc-path params)
        source (source-of path)
        d (db*)
        fid (:id (db/file-by-path d path))
        kind-of (fn [fqn] (:kind (db/var-by-fqn d fqn)))
        toks (concat
              (keep #(semantic-token* source (:name-row %) (:name-col %) (:name-end-row %)
                                      (:name-end-col %) (var-kind->token (:kind %)))
                    (db/var-defs-in-file d fid))
              (keep #(semantic-token* source (:name-row %) (:name-col %) (:name-end-row %)
                                      (:name-end-col %) (var-kind->token (kind-of (:fqn %))))
                    (remove :interop (db/var-refs-in-file d fid)))
              (keep #(semantic-token* source (:row %) (:col %) (:end-row %) (:end-col %) :keyword)
                    (concat (db/keyword-defs-in-file d fid) (db/keyword-refs-in-file d fid)))
              (keep #(semantic-token* source (:row %) (:col %) (:end-row %) (:end-col %) :parameter)
                    (concat (db/local-defs-in-file d fid) (db/local-refs-in-file d fid))))
        sorted (->> toks
                    (sort-by (juxt first second))
                    ;; one token per start position
                    (reduce (fn [acc t]
                              (let [p (peek acc)]
                                (if (and p (= (first p) (first t))
                                         (< (second t) (+ (second p) (nth p 2))))
                                  acc
                                  (conj acc t))))
                            []))
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
  (let [path (doc-path params)
        source (source-of path)
        d (db*)
        file (db/file-by-path d path)]
    (->> (db/var-defs-in-file d (:id file))
         (mapv (fn [v]
                 (let [n (count (remove :refer (db/refs-by-fqn d (:fqn v))))]
                   {:range (lsp-range source [(:name-row v) (:name-col v)
                                              (:name-end-row v) (:name-end-col v)])
                    :command {:title (str n (if (= n 1) " reference" " references"))
                              :command ""}}))))))

(defn- handle-formatting [params]
  (let [path (doc-path params)
        source (source-of path)
        r (fmt/format-string (root) source)]
    (if (and (:ok r) (not= source (:text r)))
      (let [lines (string/split source #"\n" -1)
            last-row (count lines)]
        [{:range {:start {:line 0 :character 0}
                  :end {:line (dec last-row)
                        :character (text/line-utf16-length source last-row)}}
          :newText (:text r)}])
      [])))

(defn- reindex! [path text]
  (api/refresh-file! (root) path text)
  (publish-diagnostics path))

(defn- handle-watched [params]
  (doseq [{:keys [uri type]} (:changes params)
          :let [path (uri->path uri)]
          :when (and path (#{:clj :cljc :edn} (paths/dialect path))
                     (not (contains? (:docs @state) path)))]
    (if (= 3 type)
      (swap! api/state update :db #(db/delete-file % (root) path))
      (api/refresh-file! (root) path))))

(defn- handle [msg]
  (let [method (:method msg)
        id (:id msg)
        params (:params msg)
        respond #(rpc/respond id %)]
    (case method
      "initialize"
      (let [uri (or (:rootUri params) (get-in params [:workspaceFolders 0 :uri]))
            root (or (some-> uri paths/uri->path) (System/getProperty "user.dir"))]
        (api/ensure-db root)
        (respond {:capabilities
                  {:textDocumentSync {:openClose true :change 1 :save true}
                   :definitionProvider true
                   :referencesProvider true
                   :hoverProvider true
                   :documentSymbolProvider true
                   :workspaceSymbolProvider true
                   :documentHighlightProvider true
                   :renameProvider {:prepareProvider true}
                   :selectionRangeProvider true
                   :foldingRangeProvider true
                   :codeActionProvider true
                   :documentOnTypeFormattingProvider {:firstTriggerCharacter "\n"}
                   :signatureHelpProvider {:triggerCharacters [" " "("]}
                   :completionProvider {:resolveProvider false :triggerCharacters ["/"]}
                   :semanticTokensProvider {:legend {:tokenTypes token-types
                                                     :tokenModifiers []}
                                            :full true}
                   :codeLensProvider {:resolveProvider false}
                   :documentFormattingProvider true}
                  :serverInfo {:name "jolt-lsp" :version version/version}}))

      "initialized"
      (rpc/request "client/registerCapability"
                   {:registrations [{:id "jolt-lsp-watch"
                                     :method "workspace/didChangeWatchedFiles"
                                     :registerOptions {:watchers [{:globPattern "**/*.{clj,cljc,edn}"}]}}]})

      "textDocument/didOpen"
      (let [path (doc-path params)
            text (get-in params [:textDocument :text])]
        (swap! state assoc-in [:docs path] text)
        (if (and (db/file-by-path (db*) path)
                 (= text (try (slurp path) (catch Throwable _ nil))))
          (publish-diagnostics path)
          (reindex! path text)))

      "textDocument/didChange"
      (let [path (doc-path params)
            text (:text (last (:contentChanges params)))]
        (swap! state assoc-in [:docs path] text)
        (reindex! path text))

      "textDocument/didSave"
      (publish-diagnostics (doc-path params))

      "textDocument/didClose"
      (let [path (doc-path params)
            buffer (get-in @state [:docs path])]
        (swap! state update :docs dissoc path)
        ;; an unsaved buffer's analysis gives way to the file on disk
        (when (and buffer (not= buffer (try (slurp path) (catch Throwable _ nil))))
          (api/refresh-file! (root) path)))

      "workspace/didChangeWatchedFiles" (handle-watched params)

      "textDocument/definition" (respond (handle-definition params))
      "textDocument/references" (respond (handle-references params))
      "textDocument/hover" (respond (handle-hover params))
      "textDocument/documentSymbol" (respond (handle-document-symbols params))
      "workspace/symbol" (respond (handle-workspace-symbols params))
      "textDocument/rename"
      (let [r (rename-result params)]
        (if (:ok r)
          (respond {:changes (into {}
                                   (map (fn [[p edits]]
                                          [(path->uri p) (edits->lsp (source-of p) edits)]))
                                   (:edits r))})
          (rpc/respond-error id -32803 (:reason r))))
      "textDocument/selectionRange" (respond (handle-selection params))
      "textDocument/completion" (respond (handle-completion params))
      "textDocument/documentHighlight" (respond (handle-highlight params))
      "textDocument/foldingRange" (respond (handle-folding params))
      "textDocument/onTypeFormatting" (respond (handle-on-type params))
      "textDocument/codeAction" (respond (handle-code-actions params))
      "textDocument/signatureHelp" (respond (handle-signature params))
      "textDocument/prepareRename" (respond (handle-prepare-rename params))
      "textDocument/semanticTokens/full" (respond (handle-semantic-tokens params))
      "textDocument/codeLens" (respond (handle-code-lens params))
      "textDocument/formatting" (respond (handle-formatting params))
      "shutdown" (respond nil)
      "exit" (System/exit 0)
      ;; responses to our own requests carry no method
      nil nil
      (when id (rpc/respond-error id -32601 (str "unknown method " method))))))

(defn -main [& _]
  (loop []
    (when-let [msg (rpc/read-message)]
      (try
        (handle msg)
        (catch Throwable e
          (when (and (:id msg) (:method msg))
            (rpc/respond-error (:id msg) -32603 (str e)))))
      (recur)))
  (System/exit 0))
