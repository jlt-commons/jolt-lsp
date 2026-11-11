(ns jolt-lsp.diagnostics
  "Built-in findings for one file, computed from the index.

  The unused-namespace rules follow clj-kondo: a libspec with no :as or
  :refer (`[app.routes]`, `app.routes`) is a side-effect load and is never
  unused; an alias counts as used by `::alias/kw` keywords too."
  (:require [clojure.string :as string]
            [jolt-lsp.custom :as custom]
            [jolt-lsp.db :as db]
            [jolt-lsp.kondo :as kondo]
            [jolt-lsp.layer :as layer]
            [jolt-lsp.syntax :as syntax]))

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

(defn unresolved-namespace [refs]
  (for [r refs
        :when (and (:qualified r) (:unresolved r) (:prefix r) (checked-ref? r))]
    (diag (row-range r) :error :unresolved-namespace
          (str "Unknown namespace: " (:prefix r)))))

(defn unresolved-symbol [refs]
  (for [r refs
        :when (and (not (:qualified r)) (:unresolved r) (checked-ref? r))]
    (diag (row-range r) :error :unresolved-symbol
          (str "Unresolved symbol: " (:text r)))))

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
  #{:field :fn :letfn :method-param :catch})

(defn unused-bindings [locals local-refs]
  (let [used (set (map :local-id local-refs))]
    (for [l locals
          :when (and (not (contains? used (:id l)))
                     (not (quiet-local-kinds (:kind l)))
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

(defn file-diagnostics
  "Diagnostics for one indexed file. opts: :source (the text, when it is
  not the file on disk), :kondo, :custom-linters, :clj-depend, :root."
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
           source (when (:parse-error file)
                    (or (:source opts) (try (slurp path) (catch Throwable _ ""))))]
       (->> (concat (when source (syntax-errors source))
                    (unresolved-namespace refs)
                    (unresolved-symbol refs)
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
