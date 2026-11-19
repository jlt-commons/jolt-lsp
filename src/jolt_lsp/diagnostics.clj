(ns jolt-lsp.diagnostics
  "Built-in findings for one file, computed from the index.

  The unused-namespace rules follow clj-kondo: a libspec with no :as or
  :refer (`[app.routes]`, `app.routes`) is a side-effect load and is never
  unused; an alias counts as used by `::alias/kw` keywords too."
  (:require [clojure.string :as string]
            [jolt-lsp.arity :as arity]
            [jolt-lsp.core-info :as core-info]
            [jolt-lsp.custom :as custom]
            [jolt-lsp.db :as db]
            [jolt-lsp.kondo :as kondo]
            [jolt-lsp.layer :as layer]
            [jolt-lsp.syntax :as syntax]
            [jolt-lsp.types :as types]))

(defn- diag [r severity code message]
  {:row (nth r 0) :col (nth r 1)
   :end-row (nth r 2) :end-col (nth r 3)
   :severity severity
   :code code
   :message message
   :source :jolt-lsp})

(defn- row-range [r]
  [(:row r) (:col r) (:end-row r) (:end-col r)])

(defn- checked-ref?
  "A reference the unresolved checks look at: not interop, not inside a
  syntax-quote, not in an inactive reader branch, not a refer site."
  [r]
  (not (or (:interop r) (:quoted r) (:inactive r) (:refer r))))

(defn unresolved-namespace
  "Qualified names whose prefix is no alias or required namespace. A
  namespace the index has is only not required here (loaded elsewhere,
  it works, but nothing guarantees the order): a warning, worded as
  clj-kondo words it. One nobody defines is an error. `ns-state` as for
  unresolved-var; without it every one is an error."
  ([refs] (unresolved-namespace refs (constantly nil)))
  ([refs ns-state]
   (for [r refs
         :when (and (:qualified r) (:unresolved r) (:prefix r) (checked-ref? r))]
     (if (ns-state (:prefix r))
       (diag (row-range r) :warning :unresolved-namespace
             (str "Unresolved namespace " (:prefix r) ". Are you missing a require?"))
       (diag (row-range r) :error :unresolved-namespace
             (str "Unknown namespace: " (:prefix r)))))))

(defn unresolved-symbol [refs]
  (for [r refs
        :when (and (not (:qualified r)) (:unresolved r) (checked-ref? r))]
    (diag (row-range r) :error :unresolved-symbol
          (str "Unresolved symbol: " (:text r)))))

(defn- var-facts
  "What the index (or, for a core var, the running Jolt) knows about the
  var an fqn names, or nil."
  [var-of fqn]
  (or (var-of fqn) (core-info/lookup fqn)))

(defn invalid-arity
  "Calls whose argument count no arity of the called var takes. `var-of`
  looks a var up by fqn."
  [refs var-of]
  (for [r refs
        :let [n (:arity r)]
        :when (and n (:fqn r) (checked-ref? r))
        :let [v (var-facts var-of (:fqn r))
              info {:fixed-arities (set (:fixed-arities v))
                    :varargs-min-arity (:varargs-min-arity v)}]
        :when (and v (arity/known? info) (not (arity/matches? info n)))]
    (diag (row-range r) :error :invalid-arity
          (arity/arity-message info (:fqn r) n))))

(defn- fqn-ns [fqn]
  (when-let [i (string/index-of fqn "/")] (subs fqn 0 i)))

(defn unresolved-var
  "Qualified names into a namespace that is known (indexed, or a loaded
  core namespace) but has no such var. A namespace with a def-like macro
  the walk cannot see into is left alone: the var may be made there.
  `ns-state` answers :closed, :open or nil (unknown) for a namespace."
  [refs var-of ns-state]
  (for [r refs
        :let [fqn (:fqn r)]
        :when (and (:qualified r) fqn (not (:unresolved r)) (checked-ref? r)
                   (nil? (var-facts var-of fqn))
                   (= :closed (ns-state (fqn-ns fqn))))]
    (diag (row-range r) :warning :unresolved-var
          (str "Unresolved var: " (:text r)))))

(defn private-call
  "Uses of another namespace's private var, other than through #' or a
  syntax-quote."
  [refs var-of]
  (for [r refs
        :let [v (some-> (:fqn r) var-of)]
        :when (and v (:private v) (checked-ref? r) (not (:var-quoted r)) (not (:dsl r))
                   (not= (:ns v) (:ns r)))]
    (diag (row-range r) :error :private-call
          (str "#'" (:fqn r) " is private"))))

(defn deprecated-var
  "Uses of a deprecated var, outside its own definition."
  [refs var-of]
  (for [r refs
        :let [v (some->> (:fqn r) (var-facts var-of))]
        :when (and v (:deprecated v) (checked-ref? r) (not= (:caller r) (:fqn r)))]
    (diag (row-range r) :warning :deprecated-var
          (str "#'" (:fqn r) " is deprecated"))))

(defn type-mismatch
  "Arguments to a core fn whose type, as written, its spec does not take."
  [refs]
  (for [r refs
        :when (and (:arg-tags r) (:fqn r) (checked-ref? r))
        m (types/mismatches (:fqn r) (:arg-tags r))
        :let [ar (get (:arg-ranges r) (:index m))]
        :when ar]
    (diag ar :error :type-mismatch (:message m))))

(defn walk-findings
  "Findings the analysis walk decided (local calls, recur, overloads)."
  [findings]
  (for [f findings]
    (diag (row-range f) (:severity f) (:code f) (:message f))))

(defn- usage
  "What a file uses: written prefixes, resolved fqns, keyword namespaces."
  [refs keyword-refs]
  (let [uses (remove :refer refs)]
    {:prefixes (set (keep :prefix uses))
     :fqn-ns (set (keep #(some-> (:fqn %) (string/split #"/" 2) first) uses))
     :refer-uses (set (map (juxt :fqn :text) (remove :qualified uses)))
     :keyword-ns (set (keep #(let [f (:fqn %)]
                               (when-let [i (string/index-of f "/")]
                                 (subs f 1 i)))
                            keyword-refs))}))

(defn require-used?
  "Whether a require row is used, by alias, fully-qualified name, referred
  var, or namespaced keyword."
  [{:keys [prefixes fqn-ns refer-uses keyword-ns]} refers req]
  (let [{:keys [alias target]} req]
    (or (:bare req)
        (contains? prefixes target)
        (and alias (contains? prefixes alias))
        (contains? keyword-ns target)
        (and (or (:refer-all req) (not alias)) (contains? fqn-ns target))
        (some #(contains? refer-uses [(:target %) (:name %)])
              (filter #(string/starts-with? (:target %) (str target "/")) refers)))))

(defn unused-namespaces [requires refers u]
  (for [req requires
        :when (and (:target req) (not (require-used? u refers req)))]
    (diag (row-range req) :warning :unused-namespace
          (str "Unused namespace: " (:target req)))))

(defn unused-refers
  "Referred names nothing in the file uses. `refer-sites` are the var-ref
  rows at the names inside :refer vectors."
  [refer-sites u]
  (for [site refer-sites
        :when (not (contains? (:refer-uses u) [(:fqn site) (:text site)]))]
    (diag (row-range site) :warning :unused-referred-var
          (str "Unused referred var: " (:fqn site)))))

(defn duplicate-requires [requires]
  (->> requires
       (group-by :target)
       (mapcat (fn [[target rs]]
                 (for [r (rest (sort-by (juxt :row :col) rs))]
                   (diag (row-range r) :warning :duplicate-require
                         (str "Duplicate require: " target)))))))

(def ^:private quiet-local-kinds
  "Locals whose disuse is normal: record fields, a fn's own name, method
  params fixed by a protocol, catch bindings."
  #{:field :fn :letfn :method-param :catch :comment-def})

(defn unused-bindings [locals local-refs]
  (let [used (set (map :local-id local-refs))]
    (for [l locals
          :when (and (not (contains? used (:id l)))
                     (not (quiet-local-kinds (:kind l)))
                     (not (:generated l))
                     (not (string/starts-with? (:name l) "_")))]
      (diag (row-range l) :warning :unused-binding
            (str "Unused binding: " (:name l))))))

(defn unused-private-vars [var-defs var-refs]
  (let [used (set (map :fqn var-refs))]
    (for [v var-defs
          :when (and (:private v)
                     (not= :declare (:kind v))
                     (not= :protocol-method (:kind v))
                     (not (contains? used (:fqn v))))]
      (diag [(:name-row v) (:name-col v) (:name-end-row v) (:name-end-col v)]
            :warning :unused-private-var
            (str "Unused private var: " (:name v))))))

(defn syntax-errors
  "The reader's complaint about a file, placed and explained."
  [source]
  (when-let [d (syntax/diagnose source)]
    (let [line (or (:line d) 1)
          col (or (:col d) 1)]
      [(assoc (diag [line col line (inc col)] :error :syntax-error
                    (or (syntax/explain d) "the file does not read"))
              :problem (:reason d))])))

(defn- ns-state
  "For a namespace name: :closed when every var it has is indexed, :open
  when a file of it defines vars the walk cannot see, nil when unknown."
  [db]
  (fn [nsn]
    (let [files (db/ns-files db nsn)]
      (cond
        (seq files) (if (some :open-ns files) :open :closed)
        (core-info/loaded-ns? nsn) :closed
        :else nil))))

(defn apply-level
  "A built-in finding at the level the config gives its code (kondo's
  `{:linters {:code {:level ...}}}`), or nil when the level is :off."
  [levels d]
  (let [level (and (= :jolt-lsp (:source d)) (get levels (:code d)))]
    (cond
      (nil? level) d
      (= :off level) nil
      :else (assoc d :severity level))))

(defn file-diagnostics
  "Diagnostics for one indexed file. opts: :source (the text, when it is
  not the file on disk), :kondo, :custom-linters, :clj-depend, :root,
  :levels (a severity, or :off, per code)."
  ([db path] (file-diagnostics db path {}))
  ([db path opts]
   (when-let [file (db/file-by-path db path)]
     (let [path (:path file)
           fid (:id file)
           refs (db/var-refs-in-file db fid)
           kw-refs (db/keyword-refs-in-file db fid)
           requires (db/file-requires db path)
           refers (db/file-refers db path)
           u (usage refs kw-refs)
           var-of #(db/var-by-fqn db %)
           source (when (:parse-error file)
                    (or (:source opts) (try (slurp path) (catch Throwable _ ""))))]
       (->> (concat (when source (syntax-errors source))
                    (unresolved-namespace refs (ns-state db))
                    (unresolved-symbol refs)
                    (invalid-arity refs var-of)
                    (unresolved-var refs var-of (ns-state db))
                    (private-call refs var-of)
                    (deprecated-var refs var-of)
                    (type-mismatch refs)
                    (walk-findings (db/findings-in-file db fid))
                    (unused-namespaces requires refers u)
                    (unused-refers (filter :refer refs) u)
                    (duplicate-requires requires)
                    (unused-bindings (db/local-defs-in-file db fid)
                                     (db/local-refs-in-file db fid))
                    (unused-private-vars (db/var-defs-in-file db fid) refs)
                    (when (:kondo opts) (kondo/lint [path]))
                    (when (seq (:custom-linters opts))
                      (custom/lint db (:root opts) path opts))
                    (when (:clj-depend opts)
                      (layer/file-diagnostics db path (:clj-depend opts))))
            (keep #(apply-level (:levels opts) %))
            (map #(assoc % :file path))
            (reduce (fn [m d]
                      (assoc m [(:row d) (:col d) (:code d)] d))
                    {})
            vals
            (sort-by (juxt :row :col))
            vec)))))

(defn project-diagnostics
  "Diagnostics for every project file (dependencies are not linted)."
  ([db] (project-diagnostics db {}))
  ([db opts]
   (mapcat (fn [f] (file-diagnostics db (:path f) opts))
           (remove :external (db/all-files db)))))
