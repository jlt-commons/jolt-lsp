;; The location annotation (annotate) and the macroexpand call shape follow
;; clj-kondo's clj-kondo.impl.hooks (https://github.com/clj-kondo/clj-kondo),
;; Copyright (c) Michiel Borkent, Eclipse Public License 1.0. See NOTICE.
(ns jolt-lsp.hooks
  "clj-kondo hooks, run natively.

  clj-kondo evaluates hook code in SCI; Jolt is a Clojure, so the hook
  namespaces are loaded into this process with clj-kondo.hooks-api
  provided by jolt-lsp. Two kinds, as clj-kondo configures them:

    {:hooks {:analyze-call {my.lib/defthing hooks.my-lib/defthing}
             :macroexpand  {my.lib/with-x  hooks.my-lib/with-x}}}

  An :analyze-call hook gets {:node rewrite-clj-node ...} and returns
  {:node new-node}; a :macroexpand hook is a macro the call is expanded
  with. Either way the walk analyzes the returned form in place of the
  call. Nodes cross over as rewrite-clj nodes with their location in the
  metadata, and come back as jolt-lsp.parse nodes; one a hook made takes
  the location of the nearest written node around it and is marked
  :generated, which the checks leave alone.

  Hook code is project code, the same trust as :custom-linters; set
  `:hooks false` in .jolt-lsp/config.edn to never load it."
  (:require [babashka.fs :as fs]
            [clj-kondo.hooks-api :as api]
            [clojure.string :as string]
            [jolt-lsp.parse :as parse]
            [jolt-lsp.tree :as tree]
            [rewrite-clj.node :as n]))

;; ---------------------------------------------------------------------------
;; loading hook namespaces

(defonce ^:private loaded (atom {}))      ; file -> content hash
(defonce ^:private lock (Object.))

(defn- ns->rel [ns-sym]
  (-> (str ns-sym) (string/replace "-" "_") (string/replace "." "/")))

(defn- find-file
  "The source of ns-sym under one of the config dirs, as clj-kondo looks:
  .clj_kondo, then .clj, then .cljc."
  [dirs ns-sym]
  (some (fn [d]
          (some (fn [ext]
                  (let [f (str d "/" (ns->rel ns-sym) ext)]
                    (when (fs/regular-file? f) f)))
                [".clj_kondo" ".clj" ".cljc"]))
        dirs))

(defn- required-nses
  "The namespaces a source's ns form requires."
  [src]
  (let [form (try (read-string src) (catch Throwable _ nil))]
    (when (and (seq? form) (= 'ns (first form)))
      (for [clause (drop 2 form)
            :when (and (seq? clause) (= :require (first clause)))
            lib (rest clause)
            :let [s (cond (symbol? lib) lib
                          (and (vector? lib) (symbol? (first lib))) (first lib))]
            :when s]
        s))))

(defn- load-ns*
  [dirs ns-sym seen]
  (when-let [f (find-file dirs ns-sym)]
    (when-not (contains? @seen f)
      (swap! seen conj f)
      (let [src (slurp f)
            h (hash src)]
        ;; helpers the hook requires from the config dirs load first, since
        ;; they are not on the classpath
        (doseq [dep (required-nses src)]
          (load-ns* dirs dep seen))
        (when-not (= h (get @loaded f))
          (load-file f)
          (swap! loaded assoc f h))))
    f))

(defn load-ns!
  "Load ns-sym from the config dirs (again when its file changed).
  Returns the file, or nil when no config dir has it."
  [dirs ns-sym]
  (locking lock
    (load-ns* dirs ns-sym (atom #{}))))

(defn- hook-var
  "The var a hook symbol names, loading its namespace first."
  [dirs hook-sym]
  (let [hook-sym (symbol hook-sym)
        nsn (symbol (namespace hook-sym))]
    (load-ns! dirs nsn)
    (locking lock
      (when (find-ns nsn)
        (ns-resolve (find-ns nsn) (symbol (name hook-sym)))))))

;; ---------------------------------------------------------------------------
;; jolt-lsp.parse nodes -> rewrite-clj nodes

(defn- pos [node]
  {:row (:row node) :col (:col node) :end-row (:end-row node) :end-col (:end-col node)})

(defn- keyword-of [text]
  (let [auto? (string/starts-with? text "::")
        body (subs text (if auto? 2 1))
        i (string/index-of body "/")
        k (if (and i (pos? i)) (keyword (subs body 0 i) (subs body (inc i))) (keyword body))]
    (n/keyword-node k auto?)))

(declare ->rw)

(defn- opaque
  "A node with no rewrite-clj counterpart here: a token standing for it.
  ->rw puts the original in its metadata, so it comes back unchanged."
  [_node]
  (n/token-node (symbol "__jolt-lsp-opaque__")))

(defn- rw-node [node]
  (let [kids (map ->rw (tree/children node))
        t (tree/text node)]
    (case (:tag node)
      :token (case (:kind node)
               :symbol (n/token-node (symbol t))
               (:number :literal :char) (let [v (try (read-string t) (catch Throwable _ ::bad))]
                                          (if (= ::bad v) (opaque node) (n/token-node v t)))
               (opaque node))
      :keyword (keyword-of t)
      :string (n/string-node (or (tree/sexpr node) ""))
      :regex (n/regex-node (subs t 2 (dec (count t))))
      :list (n/list-node kids)
      :vector (n/vector-node kids)
      :map (n/map-node kids)
      :set (n/set-node kids)
      :fn (n/fn-node kids)
      :quote (n/quote-node kids)
      :syntax-quote (n/syntax-quote-node kids)
      :unquote (n/unquote-node kids)
      :unquote-splicing (n/unquote-splicing-node kids)
      :deref (n/deref-node kids)
      :var (n/var-node kids)
      :meta (n/meta-node kids)
      :uneval (n/uneval-node kids)
      (opaque node))))

(defn ->rw
  "A rewrite-clj node for a jolt-lsp.parse node, located by metadata."
  [node]
  (with-meta (rw-node node) (assoc (pos node) ::orig node)))

;; ---------------------------------------------------------------------------
;; rewrite-clj nodes -> jolt-lsp.parse nodes

(def ^:private ws-tags #{:whitespace :newline :comma :comment})

(defn- leaf [tag kind text p]
  (assoc (parse/->Node tag kind text 0 (count text)
                       (:row p) (:col p) (:end-row p) (:end-col p) nil nil)
         :generated (boolean (:generated p))))

(def ^:private delimiters
  {:list ["(" ")"] :vector ["[" "]"] :map ["{" "}"] :set ["#{" "}"] :fn ["#(" ")"]
   :quote ["'" ""] :syntax-quote ["`" ""] :unquote ["~" ""] :unquote-splicing ["~@" ""]
   :deref ["@" ""] :var ["#'" ""] :meta ["^" ""] :uneval ["#_" ""] :reader-macro ["#" ""]})

(defn- coll
  "A collection node whose text is its children's, spaced as the reader
  would read them back (rewrite-clj prints children with no whitespace
  between them when a hook built the list)."
  [tag p kids]
  (let [kids (vec kids)
        [open close] (get delimiters tag ["(" ")"])
        text (str open (string/join " " (map tree/text kids)) close)]
    (assoc (parse/->Node tag nil text 0 (count text)
                         (:row p) (:col p) (:end-row p) (:end-col p) kids kids)
           :generated (boolean (:generated p)))))

(defn- token-kind [v]
  (cond (symbol? v) :symbol
        (number? v) :number
        (char? v) :char
        (or (nil? v) (true? v) (false? v)) :literal
        :else :symbol))

(defn ->parse
  "A jolt-lsp.parse node for a rewrite-clj node. `outer` is the location
  of the nearest located node around it, which an unlocated node takes
  (marked :generated), as clj-kondo's annotate does."
  [rw outer]
  (let [m (meta rw)
        orig (::orig m)
        p (if (:row m)
            (assoc (select-keys m [:row :col :end-row :end-col])
                   :generated (boolean (or (:clj-kondo.impl/generated rw)
                                           (:clj-kondo.impl/generated m))))
            (assoc outer :generated true))
        t (n/tag rw)]
    (cond
      ;; written leaves come back exactly as they were read (a hook may
      ;; copy a call's metadata onto a new node, so the text must agree)
      (and orig (not (seq (:children rw))) (not (:generated p))
           (not (tree/branch? orig)) (= (n/string rw) (tree/text orig))) orig
      (and orig (= "__jolt-lsp-opaque__" (str (:value rw)))) orig
      (instance? rewrite_clj.node.keyword.KeywordNode rw) (leaf :keyword :keyword (n/string rw) p)
      (instance? rewrite_clj.node.stringz.StringNode rw) (leaf :string :string (n/string rw) p)
      (= :regex t) (leaf :regex :string (n/string rw) p)
      (= :token t) (leaf :token (token-kind (:value rw)) (n/string rw) p)
      (contains? #{:list :vector :map :set :fn :quote :syntax-quote :unquote
                   :unquote-splicing :deref :var :meta :uneval :reader-macro} t)
      (coll t p (keep #(when-not (ws-tags (n/tag %)) (->parse % p)) (:children rw)))
      :else (leaf :token :symbol (n/string rw) p))))

;; ---------------------------------------------------------------------------
;; forms with locations, for :macroexpand hooks

(defn- located [x p]
  (try (vary-meta x merge (select-keys p [:row :col :end-row :end-col]))
       (catch Throwable _ x)))

(defn node->form
  "The value a parse node reads as, with symbols and collections carrying
  their location in metadata, as clj-kondo hands a macroexpand hook."
  [node]
  (let [kids (tree/children node)
        p (pos node)]
    (case (:tag node)
      :list (located (apply list (map node->form kids)) p)
      :vector (located (mapv node->form kids) p)
      :set (located (set (map node->form kids)) p)
      :map (located (try (apply hash-map (map node->form kids)) (catch Throwable _ {})) p)
      :quote (list 'quote (node->form (first kids)))
      :meta (node->form (last kids))
      :token (if (= :symbol (:kind node))
               (located (symbol (tree/text node)) p)
               (tree/sexpr node))
      (tree/sexpr node))))

(defn form->rw
  "A rewrite-clj node for an expansion, keeping the locations its symbols
  and collections carry."
  [x]
  (let [m (meta x)
        node (cond
               (seq? x) (n/list-node (map form->rw x))
               (vector? x) (n/vector-node (mapv form->rw x))
               (map? x) (n/map-node (mapcat (fn [[k v]] [(form->rw k) (form->rw v)]) x))
               (set? x) (n/set-node (map form->rw x))
               (keyword? x) (n/keyword-node x)
               (string? x) (n/string-node x)
               (symbol? x) (n/token-node x)
               :else (n/coerce x))]
    (if (:row m)
      (with-meta node (select-keys m [:row :col :end-row :end-col]))
      node)))

;; ---------------------------------------------------------------------------
;; running a hook

(defn- finding [call-pos m]
  (let [row (or (:row m) (:row call-pos))
        col (or (:col m) (:col call-pos))]
    {:range [row col (or (:end-row m) row) (or (:end-col m) (inc col))]
     :code (keyword (or (some-> (:type m) name) "hook"))
     :severity (case (some-> (:level m) name)
                 "error" :error
                 "info" :info
                 :warning)
     :message (or (:message m) "hook finding")}))

(defn hook-for
  "{:kind :analyze-call|:macroexpand :hook sym} configured for a call
  head's fqn, or nil."
  [cfg fqn]
  (if-let [h (get-in cfg [:analyze-call fqn])]
    {:kind :analyze-call :hook h}
    (when-let [h (get-in cfg [:macroexpand fqn])]
      {:kind :macroexpand :hook h})))

(defn expand
  "Run the hook configured for `fqn` on a call node. `walk` is the walk
  context the API answers from: {:ns :aliases :refers :locals :heads :path}.
  Returns {:node parse-node-or-nil :findings [...]}, or nil when no hook is
  configured. A hook that throws or cannot be loaded is a :hook finding
  and no node."
  [cfg fqn node walk]
  (when-let [{:keys [kind hook]} (hook-for cfg fqn)]
    (let [findings (atom [])
          call-pos (pos node)]
      (binding [api/*ctx* {:findings findings
                           :ns (some-> (:ns walk) symbol)
                           :aliases (:aliases walk)
                           :refers (:refers walk)
                           :locals (set (keys (:locals walk)))
                           :heads (:heads walk)
                           :filename (:path walk)}]
        (try
          (let [v (hook-var (:dirs cfg) hook)
                _ (when-not v (throw (ex-info (str "hook not found: " hook) {})))
                out (case kind
                      :analyze-call
                      (:node ((deref v) {:node (->rw node) :cljc false :lang :clj
                                         :filename (:path walk) :config (:config cfg)
                                         :ns (some-> (:ns walk) symbol)}))
                      :macroexpand
                      (let [form (node->form node)]
                        (form->rw (apply (deref v) form (api/env) (rest form)))))]
            {:node (when out (->parse out call-pos))
             :findings (mapv #(finding call-pos %) @findings)})
          (catch Throwable e
            {:node nil
             :findings (conj (mapv #(finding call-pos %) @findings)
                             (finding call-pos
                                      (merge {:type :hook :level :error
                                              :message (str "hook " hook " failed: "
                                                            (or (ex-message e) e))}
                                             (select-keys (ex-data e) [:row :col :level]))))}))))))
