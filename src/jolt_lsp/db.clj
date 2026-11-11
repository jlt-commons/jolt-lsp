(ns jolt-lsp.db
  "The relatum index: build, refresh, and the read helpers the queries use."
  (:require [babashka.fs :as fs]
            [clojure.string :as string]
            [jolt-lsp.analyze :as analyze]
            [jolt-lsp.cache :as cache]
            [jolt-lsp.paths :as paths]
            [jolt-lsp.resolve :as resolve]
            [jolt-lsp.schema]
            [relatum.core :as rc]))

(declare file-by-path normalize-path)

(def ^:private special-names
  ["def" "if" "do" "let" "let*" "fn" "fn*" "loop" "loop*" "recur" "throw"
   "try" "catch" "finally" "new" "set!" "quote" "var" "." "monitor-enter"
   "monitor-exit" "deftype*" "reify*" "case*" "import*" "def*" "in-ns" "ns" "&"])

(def ^:private core-names
  (delay (vec (distinct (concat special-names
                                (sort (map name (keys (ns-publics 'clojure.core)))))))))

(defn- slurp-file [path]
  (try (slurp path) (catch Throwable _ "")))

(defn- analysis-of [path]
  (let [source (slurp-file path)]
    (if (= :edn (paths/dialect path))
      (analyze/analyze-edn source path)
      (analyze/analyze source path))))

(defn- insert-rows [db table rows]
  (if (seq rows)
    (rc/insert-many db table (vec rows))
    db))

(defn- var-priority [v]
  (if (= :declare (:kind v)) 1 0))

(defn- dedupe-vars [vars]
  (->> vars
       (sort-by (fn [v] [(if (:external v) 1 0) (var-priority v) (:path v) (:row v)]))
       (reduce (fn [seen v]
                 (if (contains? seen (:fqn v))
                   seen
                   (assoc seen (:fqn v) v)))
               {})
       vals
       vec))

(defn- resolve-occurrence [analysis all-vars ns text]
  (let [ctor-base (cond
                    (string/starts-with? text "map->") (subs text 5)
                    (string/starts-with? text "->") (subs text 2)
                    :else nil)
        ctor-fqn (when ctor-base
                   (when-let [v (get all-vars (if ns (str ns "/" ctor-base) ctor-base))]
                     (when (#{:defrecord :deftype} (:kind v))
                       (:fqn v))))
        r (if ctor-fqn
            {:kind :var :fqn ctor-fqn}
            (resolve/resolve-name analysis ns text))]
    {:fqn (:fqn r)
     :kind (:kind r)
     :unresolved (= :unresolved (:kind r))}))

(defn- ns-analysis [a vars-by-ns]
  {:ns-name (:ns a)
   :vars (get vars-by-ns (:ns a))
   :aliases (get-in a [:ns-meta :aliases])
   :refers (get-in a [:ns-meta :refers])
   :requires (get-in a [:ns-meta :requires])
   :core @core-names})

(defn- var-row [file-id v]
  {:fqn (:fqn v)
   :ns (:ns v)
   :name (:name v)
   :kind (:kind v)
   :file-id file-id
   :row (nth (:range v) 0)
   :col (nth (:range v) 1)
   :end-row (nth (:range v) 2)
   :end-col (nth (:range v) 3)
   :name-row (nth (:name-range v) 0)
   :name-col (nth (:name-range v) 1)
   :name-end-row (nth (:name-range v) 2)
   :name-end-col (nth (:name-range v) 3)
   :private (boolean (:private v))
   :deprecated false
   :doc (:doc v)
   :arglists (:arglists v)
   :params (pr-str (or (:params v) []))
   :defined-by (:defined-by v)})

(defn- local-row [id file-id local]
  (let [r (:range local)
        s (:scope-range local)]
    {:id id
     :file-id file-id
     :name (:name local)
     :kind (:kind local)
     :row (nth r 0) :col (nth r 1) :end-row (nth r 2) :end-col (nth r 3)
     :scope-row (nth s 0) :scope-col (nth s 1)
     :scope-end-row (nth s 2) :scope-end-col (nth s 3)
     :keys-destructuring (boolean (:keys-destructuring local))
     :destructured (boolean (:destructured local))}))

(defn- local-ref-row [local-id file-id ref]
  (let [r (:range ref)]
    {:local-id local-id
     :file-id file-id
     :name (:name ref)
     :row (nth r 0) :col (nth r 1) :end-row (nth r 2) :end-col (nth r 3)}))

(defn- distinct-by-fqn [ks]
  (->> ks (reduce (fn [seen k] (if (contains? seen (:fqn k)) seen (assoc seen (:fqn k) k))) {}) vals))

(defn- analysis-entry-text [path source external]
  {:path path
   :hash (hash source)
   :external (boolean external)
   :analysis (if (= :edn (paths/dialect path))
               (analyze/analyze-edn source path)
               (analyze/analyze source path))})

(defn- analysis-entry [path external]
  (analysis-entry-text path (slurp-file path) external))

(defn analyze-project [root]
  (vec (concat (map #(analysis-entry % false) (paths/source-files root))
               (map #(analysis-entry % true) (paths/dep-files root)))))

(def ^:private empty-rows
  {:file [] :namespace [] :ns-alias [] :ns-require [] :ns-refer []
   :ns-import [] :var-def [] :var-ref [] :local-def [] :local-ref []
   :keyword-def [] :keyword-ref [] :call-edge [] :ns-dep []})

(defn- merge-rows [a b]
  (merge-with into a b))

(defn- file-rows
  "Rows for one analysis. Returns {:rows ... :next-local n}."
  [a file-id all-vars vars-by-ns local-start]
  (let [ns (:ns a)
        a-res (ns-analysis a vars-by-ns)
        local-counter (atom local-start)
        local-ids (into {}
                        (for [l (:locals a)]
                          [[file-id (:local-id l)] (swap! local-counter inc)]))
        resolved (mapv (fn [occ]
                         (let [r (resolve-occurrence a-res all-vars ns (:text occ))]
                           (assoc occ :fqn (:fqn r) :unresolved (:unresolved r))))
                       (:occurrences a))]
    {:next-local @local-counter
     :rows
     (-> empty-rows
         (update :file conj
                 {:id file-id
                  :path (:file-path a)
                  :uri (paths/file-uri (:file-path a))
                  :ns ns
                  :dialect (paths/dialect (:file-path a))
                  :project (:project a)
                  :external (boolean (:external a))
                  :mtime 0
                  :hash nil})
         (update :namespace conj {:id file-id :file-id file-id :name ns})
         (update :ns-alias into
                 (for [[alias target] (get-in a [:ns-meta :aliases])]
                   {:ns-id file-id :alias alias :target target}))
         (update :ns-require into
                 (for [target (distinct (get-in a [:ns-meta :requires]))
                       :let [spec (some #(when (= (:target %) target) %)
                                        (:require-specs (:ns-meta a)))
                             r (or (:range spec) [1 1 1 1])]]
                   {:ns-id file-id
                    :target target
                    :alias (some (fn [[al tg]] (when (= tg target) al))
                                 (get-in a [:ns-meta :aliases]))
                    :refer-all (boolean (some #{target} (get-in a [:ns-meta :refer-all])))
                    :as-alias (boolean (some #{target} (get-in a [:ns-meta :as-aliases])))
                    :row (nth r 0) :col (nth r 1)
                    :end-row (nth r 2) :end-col (nth r 3)}))
         (update :ns-refer into
                 (for [[name target] (get-in a [:ns-meta :refers])]
                   {:ns-id file-id :name name :target target}))
         (update :ns-import into
                 (for [[simple fq] (get-in a [:ns-meta :imports])]
                   {:ns-id file-id :simple simple :fq fq}))
         (update :var-def into
                 (->> (:vars a)
                      (filter (fn [v] (identical? (get all-vars (:fqn v)) v)))
                      (map (partial var-row file-id))))
         (update :var-ref into
                 (for [site (get-in a [:ns-meta :refer-sites])]
                   (let [r (:range site)]
                     {:file-id file-id
                      :ns ns
                      :text (:name site)
                      :name (:name site)
                      :prefix nil
                      :fqn (:target site)
                      :qualified false
                      :in-call false
                      :caller nil
                      :unresolved false
                      :row (nth r 0) :col (nth r 1)
                      :end-row (nth r 2) :end-col (nth r 3)
                      :name-row (nth r 0) :name-col (nth r 1)
                      :name-end-row (nth r 2) :name-end-col (nth r 3)})))
         (update :var-ref into
                 (for [occ resolved]
                   (let [r (:range occ) nr (:name-range occ)]
                     {:file-id file-id
                      :ns ns
                      :text (:text occ)
                      :name (:name occ)
                      :prefix (:prefix occ)
                      :fqn (:fqn occ)
                      :qualified (boolean (:qualified occ))
                      :in-call (boolean (:in-call occ))
                      :caller (:caller occ)
                      :unresolved (boolean (:unresolved occ))
                      :row (nth r 0) :col (nth r 1)
                      :end-row (nth r 2) :end-col (nth r 3)
                      :name-row (nth nr 0) :name-col (nth nr 1)
                      :name-end-row (nth nr 2) :name-end-col (nth nr 3)})))
         (update :local-def into
                 (map (fn [l] (local-row (get local-ids [file-id (:local-id l)])
                                         file-id l))
                      (:locals a)))
         (update :local-ref into
                 (for [ref (:local-refs a)]
                   (local-ref-row (get local-ids [file-id (:local-id ref)])
                                  file-id ref)))
         (update :keyword-def into
                 (for [k (distinct-by-fqn (filter :def? (:keywords a)))]
                   {:fqn (:fqn k)
                    :file-id file-id
                    :kind (if (= :auto (:notation k)) :keyword :integrant-key)
                    :row (nth (:range k) 0) :col (nth (:range k) 1)
                    :end-row (nth (:range k) 2) :end-col (nth (:range k) 3)}))
         (update :keyword-ref into
                 (for [k (remove :def? (:keywords a))]
                   {:fqn (:fqn k)
                    :file-id file-id
                    :notation (:notation k)
                    :row (nth (:range k) 0) :col (nth (:range k) 1)
                    :end-row (nth (:range k) 2) :end-col (nth (:range k) 3)}))
         (update :call-edge into
                 (for [occ resolved
                       :when (and (:in-call occ) (:fqn occ) (:caller occ))]
                   (let [r (:name-range occ)]
                     {:caller (:caller occ)
                      :callee (:fqn occ)
                      :file-id file-id
                      :row (nth r 0) :col (nth r 1)
                      :end-row (nth r 2) :end-col (nth r 3)})))
         (update :ns-dep into
                 (for [target (distinct (get-in a [:ns-meta :requires]))]
                   {:from-ns ns :to-ns target :kind :require})))}))

(defn- analysis-maps [entries]
  (mapv (fn [e]
          (-> (:analysis e)
              (assoc :file-path (:path e) :external (:external e) :project (:project e))
              (update :vars #(mapv (fn [v] (assoc v :external (:external e))) %))))
        entries))

(defn build-from-analyses
  "Index these analyses. Returns the relatum db."
  [root entries]
  (let [files (mapv :path entries)
        analyses (analysis-maps entries)
        file-ids (zipmap files (iterate inc 1))
        all-vars (into {} (map (juxt :fqn identity)
                               (dedupe-vars (mapcat :vars analyses))))
        vars-by-ns (group-by :ns (vals all-vars))
        [rows _]
        (reduce (fn [[acc next-local] a]
                  (let [r (file-rows (assoc a :project root)
                                     (get file-ids (:file-path a))
                                     all-vars vars-by-ns next-local)]
                    [(merge-rows acc (:rows r)) (:next-local r)]))
                [empty-rows 0]
                analyses)]
    (-> (rc/create-db)
        (insert-rows :file (:file rows))
        (insert-rows :namespace (:namespace rows))
        (insert-rows :ns-alias (:ns-alias rows))
        (insert-rows :ns-require (:ns-require rows))
        (insert-rows :ns-refer (:ns-refer rows))
        (insert-rows :ns-import (:ns-import rows))
        (insert-rows :var-def (:var-def rows))
        (insert-rows :var-ref (:var-ref rows))
        (insert-rows :local-def (:local-def rows))
        (insert-rows :local-ref (:local-ref rows))
        (insert-rows :keyword-def (:keyword-def rows))
        (insert-rows :keyword-ref (:keyword-ref rows))
        (insert-rows :call-edge (:call-edge rows))
        (insert-rows :ns-dep (:ns-dep rows)))))

(defn build
  "Index every source file under root from scratch. Returns the relatum db."
  [root]
  (build-from-analyses root (analyze-project root)))

(defn build-cached
  "Reuse cached analyses whose content hash is unchanged, then index."
  [root]
  (let [cached (or (cache/read-cache root) {})
        all (concat (map #(vector % false) (paths/source-files root))
                    (map #(vector % true) (paths/dep-files root)))
        entries
        (mapv (fn [[path external]]
                (let [source (slurp-file path)
                      h (hash source)
                      c (get cached path)]
                  (if (and c (= h (:hash c)) (= external (:external c)))
                    {:path path :hash h :external external :analysis (:analysis c)}
                    {:path path
                     :hash h
                     :external external
                     :analysis (if (= :edn (paths/dialect path))
                                 (analyze/analyze-edn source path)
                                 (analyze/analyze source path))})))
              all)]
    (cache/write-cache! root entries)
    (build-from-analyses root entries)))

(defn- next-local-id [db]
  (inc (reduce max 0 (map :id (rc/rows db :local-def)))))

(defn update-file-text
  "Patch one file's rows in the index from `source`. Falls back to a cached
  rebuild when the cache cannot account for the project."
  [db root path source]
  (let [path (normalize-path path)]
    (try
      (let [cached (or (cache/read-cache root) {})]
        (if (empty? cached)
          (build-cached root)
          (let [entry (analysis-entry-text path source false)
                cached (assoc cached path (select-keys entry [:hash :external :analysis]))
                entries (mapv (fn [[p e]] (assoc e :path p)) cached)
                analyses (analysis-maps entries)
                all-vars (into {} (map (juxt :fqn identity)
                                       (dedupe-vars (mapcat :vars analyses))))
                vars-by-ns (group-by :ns (vals all-vars))
                file (file-by-path db path)
                a (some #(when (= (:file-path %) path) %) analyses)]
            (cond
              (nil? a) (build-cached root)
              (nil? file) (do (cache/write-cache! root entries)
                              (build-from-analyses root entries))
              :else
              (let [file-id (:id file)
                    old-ns (:ns file)
                    fr (file-rows (assoc a :project root) file-id all-vars vars-by-ns
                                  (next-local-id db))
                    db1 (-> db
                            (rc/delete :ns-dep [= :from-ns old-ns])
                            (rc/delete :file [= :id file-id]))]
                (cache/write-cache! root entries)
                (reduce-kv (fn [d table rows] (insert-rows d table rows))
                           db1
                           (:rows fr)))))))
    (catch Throwable _
      (build-cached root)))))

(defn update-file [db root path]
  (update-file-text db root path (slurp-file path)))

(defn delete-file [db root path]
  (let [path (normalize-path path)]
    (try
      (let [file (file-by-path db path)
          db1 (-> db
                  (rc/delete :ns-dep [= :from-ns (:ns file)])
                  (rc/delete :file [= :id (:id file)]))
          cached (or (cache/read-cache root) {})]
      (cache/write-cache! root
                          (mapv (fn [[p e]] (assoc e :path p)) (dissoc cached path)))
      db1)
      (catch Throwable _ db))))

(defn refresh [root]
  (build-cached root))

(defn normalize-path [path]
  (let [p (fs/absolutize (str path))]
    (if (fs/exists? p)
      (str (fs/canonicalize p))
      (str p))))

(defn file-by-path [db path]
  (get-in (rc/index db :file/by-path) [(normalize-path path)]))

(defn file-by-id [db id]
  (rc/row db :file id))

(defn all-files [db]
  (rc/rows db :file))

(defn var-by-fqn [db fqn]
  (get-in (rc/index db :var-def/by-fqn) [fqn]))

(defn vars-in-ns [db ns]
  (get (rc/index db :var-def/by-ns) ns))

(defn refs-by-fqn [db fqn]
  (get (rc/index db :var-ref/by-fqn) fqn))

(defn callers-of [db fqn]
  (get (rc/index db :call-edge/by-callee) fqn))

(defn callees-of [db fqn]
  (get (rc/index db :call-edge/by-caller) fqn))

(defn ns-deps-of [db ns]
  (get (rc/index db :ns-dep/by-from-ns) ns))

(defn ns-dependents-of [db ns]
  (get (rc/index db :ns-dep/by-to-ns) ns))

(defn rows-in-file [db table file-id]
  (rc/q db [[:from table] [:where [= :file-id file-id]]]))

(defn var-defs-in-file [db file-id]
  (rows-in-file db :var-def file-id))

(defn var-refs-in-file [db file-id]
  (rows-in-file db :var-ref file-id))

(defn local-defs-in-file [db file-id]
  (rows-in-file db :local-def file-id))

(defn local-refs-in-file [db file-id]
  (rows-in-file db :local-ref file-id))

(defn keyword-defs-in-file [db file-id]
  (rows-in-file db :keyword-def file-id))

(defn keyword-refs-in-file [db file-id]
  (rows-in-file db :keyword-ref file-id))

(defn file-by-ns [db ns]
  (first (get (rc/index db :file/by-ns) ns)))

(defn ns-requires-of [db ns]
  (when-let [f (file-by-ns db ns)]
    (rc/q db [[:from :ns-require] [:where [= :ns-id (:id f)]]])))

(defn ns-refers-of [db ns]
  (when-let [f (file-by-ns db ns)]
    (rc/q db [[:from :ns-refer] [:where [= :ns-id (:id f)]]])))

(defn ns-aliases-of [db ns]
  (when-let [f (file-by-ns db ns)]
    (rc/q db [[:from :ns-alias] [:where [= :ns-id (:id f)]]])))

(defn all-var-defs [db]
  (rc/rows db :var-def))

(defn all-keyword-defs [db]
  (rc/rows db :keyword-def))


(defn local-def-by-id [db id]
  (rc/row db :local-def id))

(defn local-refs-by-local-id [db id]
  (rc/q db [[:from :local-ref] [:where [= :local-id id]]]))

(defn keyword-def-by-fqn [db fqn]
  (get-in (rc/index db :keyword-def/by-fqn) [fqn]))

(defn keyword-refs-by-fqn [db fqn]
  (get (rc/index db :keyword-ref/by-fqn) fqn))

(defn all-keyword-refs [db]
  (rc/rows db :keyword-ref))
