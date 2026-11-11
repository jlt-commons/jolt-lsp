(ns jolt-lsp.db
  "The index: build, refresh, and the read helpers the queries use.

  Rows are built from analyses and kept in plain persistent maps, per file
  and in a few cross-file indexes, so a build is a fold and a file patch
  touches only that file's keys.

  Analyses are cached in memory by content hash, so patching one file never
  re-reads the project. Files are analyzed in parallel; dependency files
  shallowly (definitions only)."
  (:require [babashka.fs :as fs]
            [clojure.string :as string]
            [jolt-lsp.analyze :as analyze]
            [jolt-lsp.paths :as paths]
            [jolt-lsp.resolve :as resolve]
            [jolt-lsp.settings :as settings]
))

(declare file-by-path normalize-path)

(def ^:private special-names
  ["def" "if" "do" "let" "let*" "fn" "fn*" "loop" "loop*" "recur" "throw"
   "try" "catch" "finally" "new" "set!" "quote" "var" "." ".." "monitor-enter"
   "monitor-exit" "deftype*" "reify*" "case*" "import*" "def*" "in-ns" "ns" "&"
   "letfn*" "clojure.core/import*"])

(def ^:private core-names
  (delay (set (concat special-names
                      (map name (keys (ns-publics 'clojure.core)))))))

(def ^:private trace? (some? (System/getenv "JOLT_LSP_TRACE")))

(defmacro ^:private timed
  "Run body; with JOLT_LSP_TRACE set, print how long it took to stderr."
  [label & body]
  `(if trace?
     (let [t0# (System/currentTimeMillis)
           v# (do ~@body)]
       (binding [*out* *err*]
         (println "jolt-lsp:" ~label (- (System/currentTimeMillis) t0#) "ms"))
       v#)
     (do ~@body)))

(defn- slurp-file [path]
  (try (slurp path) (catch Throwable _ "")))

(defn- mtime [path]
  (try (fs/file-time->millis (fs/last-modified-time path)) (catch Throwable _ 0)))

(def ^:private tables
  [:file :namespace :ns-alias :ns-require :ns-refer :ns-import :var-def
   :var-ref :local-def :local-ref :keyword-def :keyword-ref :call-edge :ns-dep])

;; ---------------------------------------------------------------------------
;; analyses and the cache

(def ^:dynamic *lint* "The project's lint config, see settings/lint-config." {})

(defn- analyze-source [path source external]
  (binding [analyze/*lint-as* (:lint-as *lint*)]
    (cond
      (= :edn (paths/dialect path)) (analyze/analyze-edn source path)
      external (analyze/analyze-shallow source path)
      :else (analyze/analyze source path))))

(defn- content-hash
  "Analyses depend on the text and on :lint-as."
  [source]
  (hash [source (:lint-as *lint*)]))

(defn- analysis-entry-text [path source external]
  {:path path
   :hash (content-hash source)
   :mtime (mtime path)
   :external (boolean external)
   :analysis (analyze-source path source external)})

(defonce ^:private mem-cache (atom {}))

(defn- cached-entries
  "{path entry} for root, kept in memory for the life of the process.
  (Re-analyzing is faster than reading a serialized cache back.)"
  [root]
  (get @mem-cache root {}))

(defn- pmapv
  "mapv across the machine's cores, in chunks. Dynamic bindings the
  analysis reads are re-established in each worker."
  [f xs]
  (let [xs (vec xs)
        n (max 1 (.availableProcessors (Runtime/getRuntime)))
        size (max 8 (quot (+ (count xs) n -1) n))
        lint *lint*
        work (fn [chunk] (binding [*lint* lint] (mapv f chunk)))]
    (if (<= (count xs) size)
      (work xs)
      (into [] (mapcat deref) (mapv #(future (work %)) (partition-all size xs))))))

(defn- remember! [root entries]
  (swap! mem-cache assoc root (into {} (map (juxt :path identity)) entries)))

(defn forget!
  "Drop the in-memory cache for root (tests, a refresh from scratch)."
  [root]
  (swap! mem-cache dissoc root))

(defn analyze-project [root]
  (pmapv (fn [[p external]] (analysis-entry-text p (slurp-file p) external))
         (concat (map #(vector % false) (paths/source-files root))
                 (map #(vector % true) (paths/dep-files root)))))

;; ---------------------------------------------------------------------------
;; rows

(defn- var-priority [v]
  (if (= :declare (:kind v)) 1 0))

(defn- dedupe-vars [vars]
  (->> vars
       (sort-by (fn [v] [(if (:external v) 1 0) (var-priority v) (str (:path v))
                         (or (first (:range v)) 0)]))
       (reduce (fn [seen v]
                 (if (contains? seen (:fqn v))
                   seen
                   (assoc seen (:fqn v) v)))
               {})))

(defn- resolve-occurrence [analysis all-vars ns text]
  (let [ctor-base (cond
                    (string/starts-with? text "map->") (subs text 5)
                    (string/starts-with? text "->") (subs text 2)
                    :else nil)
        ctor-fqn (when ctor-base
                   (when-let [v (get all-vars (if ns (str ns "/" ctor-base) ctor-base))]
                     (when (#{:defrecord :deftype} (:kind v))
                       (:fqn v))))]
    (if ctor-fqn
      {:kind :var :fqn ctor-fqn}
      (resolve/resolve-name analysis ns text))))

(defn- ns-analysis [a vars-by-ns]
  (let [refer-all (get-in a [:ns-meta :refer-all])]
    {:ns-name (:ns a)
     :var-names (set (map :name (get vars-by-ns (:ns a))))
     :aliases (get-in a [:ns-meta :aliases])
     :refers (get-in a [:ns-meta :refers])
     :requires (set (get-in a [:ns-meta :requires]))
     :imports (get-in a [:ns-meta :imports])
     :refer-all (into {}
                      (for [target refer-all
                            v (get vars-by-ns target)
                            :when (not (:private v))]
                        [(:name v) (:fqn v)]))
     :core @core-names}))

(defn- r4 [m prefix r]
  (let [r (or r [1 1 1 1])
        k (fn [s] (keyword (str prefix s)))]
    (assoc m
           (k "row") (nth r 0) (k "col") (nth r 1)
           (k "end-row") (nth r 2) (k "end-col") (nth r 3))))

(defn- var-row [file-id v]
  (-> {:fqn (:fqn v)
       :ns (:ns v)
       :name (:name v)
       :kind (:kind v)
       :file-id file-id
       :private (boolean (:private v))
       :deprecated false
       :doc (:doc v)
       :arglists (:arglists v)
       :params (pr-str (or (:params v) []))
       :defined-by (:defined-by v)}
      (r4 "" (:range v))
      (r4 "name-" (:name-range v))))

(defn- ref-row [file-id ns occ]
  (-> {:file-id file-id
       :ns ns
       :text (:text occ)
       :name (:name occ)
       :prefix (:prefix occ)
       :fqn (:fqn occ)
       :qualified (boolean (:qualified occ))
       :in-call (boolean (:in-call occ))
       :caller (:caller occ)
       :unresolved (boolean (:unresolved occ))
       :interop (boolean (:interop occ))
       :quoted (boolean (:quoted occ))
       :inactive (boolean (:inactive occ))
       :refer (boolean (:refer occ))}
      (r4 "" (:range occ))
      (r4 "name-" (or (:name-range occ) (:range occ)))))

(defn- empty-rows []
  (zipmap tables (repeat [])))

(defn- file-rows
  "Rows for one analysis. Dependency files contribute only what navigation
  into them needs: the file, its namespace facts and its definitions.
  Returns {:rows ... :next-local n}."
  [a file-id all-vars vars-by-ns local-start]
  (let [ns (:ns a)
        external? (boolean (:external a))
        a-res (ns-analysis a vars-by-ns)
        local-counter (atom local-start)
        local-ids (when-not external?
                    (into {}
                          (for [l (:locals a)]
                            [(:local-id l) (swap! local-counter inc)])))
        opaque-head? (memoize
                      (fn [h]
                        (let [r (resolve-occurrence a-res all-vars ns h)
                              v (get all-vars (:fqn r))]
                          (and (= :var (:kind r))
                               (or (nil? v) (= :defmacro (:kind v)))))))
        {:keys [exclude-symbols exclude-calls exclude-namespaces]} *lint*
        head-fqn (memoize (fn [h] (:fqn (resolve-occurrence a-res all-vars ns h))))
        excluded? (fn [occ]
                    (or (contains? exclude-symbols (:text occ))
                        (and (:qualified occ) (contains? exclude-namespaces (:prefix occ)))
                        (some (fn [h]
                                (let [f (head-fqn h)]
                                  (when (contains? exclude-calls f)
                                    (let [names (get exclude-calls f)]
                                      (or (nil? names) (contains? names (:text occ)))))))
                              (:heads occ))))
        resolved (when-not external?
                   (mapv (fn [occ]
                           (let [r (resolve-occurrence a-res all-vars ns (:text occ))
                                 unresolved (= :unresolved (:kind r))]
                             (cond-> (assoc occ :fqn (:fqn r) :unresolved unresolved)
                               ;; inside a macro (or a call the index cannot
                               ;; see): its arguments may be data, not code;
                               ;; or the config excludes it, as kondo's does
                               (and unresolved (or (some opaque-head? (:heads occ))
                                                   (excluded? occ)))
                               (assoc :quoted true))))
                         (:occurrences a)))
        meta (:ns-meta a)]
    {:next-local @local-counter
     :rows
     (cond->
      (-> (empty-rows)
          (update :file conj
                  {:id file-id
                   :path (:file-path a)
                   :uri (paths/file-uri (:file-path a))
                   :ns ns
                   :dialect (paths/dialect (:file-path a))
                   :project (:project a)
                   :external external?
                   :mtime (or (:mtime a) 0)
                   :hash (str (:hash a))
                   :parse-error (when-not (:parse-ok a) (str (:parse-error a)))})
          (update :namespace conj {:id file-id :file-id file-id :name ns})
          (update :ns-alias into
                  (for [[alias target] (:aliases meta)]
                    {:ns-id file-id :alias alias :target target}))
          (update :ns-require into
                  (for [spec (:require-specs meta)]
                    (r4 {:ns-id file-id
                         :target (:target spec)
                         :alias (:alias spec)
                         :refer-all (boolean (:refer-all spec))
                         :as-alias (boolean (:as-alias spec))
                         :bare (boolean (:bare spec))}
                        "" (:range spec))))
          (update :ns-refer into
                  (for [[name target] (:refers meta)]
                    {:ns-id file-id :name name :target target}))
          (update :ns-import into
                  (for [[simple fq] (:imports meta)]
                    {:ns-id file-id :simple simple :fq fq}))
          (update :var-def into
                  (->> (:vars a)
                       (filter (fn [v] (identical? (get all-vars (:fqn v)) v)))
                       (map (partial var-row file-id))))
          (update :keyword-def into
                  (for [k (vals (reduce #(if (contains? %1 (:fqn %2)) %1 (assoc %1 (:fqn %2) %2))
                                        {} (filter :def? (:keywords a))))]
                    (r4 {:fqn (:fqn k)
                         :file-id file-id
                         :kind (if (= :auto (:notation k)) :keyword :integrant-key)}
                        "" (:range k))))
          (update :ns-dep into
                  (for [target (distinct (:requires meta))]
                    {:from-ns ns :to-ns target :kind :require})))
       (not external?)
       (->
        (update :var-ref into
                (for [site (:refer-sites meta)]
                  (ref-row file-id ns {:text (:name site) :name (:name site)
                                       :fqn (:target site) :range (:range site)
                                       :refer true})))
        (update :var-ref into (map #(ref-row file-id ns %) resolved))
        (update :local-def into
                (for [l (:locals a)
                      :let [r (:range l) s (:scope-range l)]]
                  (-> {:id (get local-ids (:local-id l))
                       :file-id file-id
                       :name (:name l)
                       :kind (:kind l)
                       :keys-destructuring (boolean (:keys-destructuring l))
                       :destructured (boolean (:destructured l))}
                      (r4 "" r)
                      (r4 "scope-" s))))
        (update :local-ref into
                (for [ref (:local-refs a)
                      :let [id (get local-ids (:local-id ref))]
                      :when id]
                  (r4 {:local-id id :file-id file-id :name (:name ref)} "" (:range ref))))
        (update :keyword-ref into
                (for [k (remove :def? (:keywords a))]
                  (r4 {:fqn (:fqn k) :file-id file-id :notation (:notation k)}
                      "" (:range k))))
        (update :call-edge into
                (for [occ resolved
                      :when (and (:in-call occ) (:fqn occ) (:caller occ))]
                  (r4 {:caller (:caller occ) :callee (:fqn occ) :file-id file-id}
                      "" (:name-range occ))))))}))

(defn- analysis-maps [entries]
  (mapv (fn [e]
          (-> (:analysis e)
              (assoc :file-path (:path e) :external (:external e)
                     :hash (:hash e) :mtime (:mtime e))
              (update :vars #(mapv (fn [v] (assoc v :external (:external e))) %))))
        entries))

;; ---------------------------------------------------------------------------
;; storage: plain persistent maps
;;
;; {:files {file-id {:file row :rows {table [row ...]}}}
;;  :ix    {index-name {key {file-id [row ...]}}}   ; or {key row} when unique
;;  :next-local n}
;;
;; Per-file tables (locals, refs in a file, requires) are read from the
;; file's own rows. The cross-file indexes map a key to the rows each file
;; contributes, so patching a file touches only that file's keys.

(def ^:private index-specs
  {:file/by-path [:file :path :unique]
   :file/by-ns [:file :ns]
   :var-def/by-fqn [:var-def :fqn :unique]
   :var-def/by-ns [:var-def :ns]
   :var-ref/by-fqn [:var-ref :fqn]
   :call-edge/by-callee [:call-edge :callee]
   :call-edge/by-caller [:call-edge :caller]
   :ns-dep/by-from-ns [:ns-dep :from-ns]
   :ns-dep/by-to-ns [:ns-dep :to-ns]
   :keyword-def/by-fqn [:keyword-def :fqn :unique]
   :keyword-ref/by-fqn [:keyword-ref :fqn]
   :local-def/by-id [:local-def :id :unique]
   :local-ref/by-local-id [:local-ref :local-id]})

(defn- fragment
  "One file's entry: its file row and its rows by table, grouped."
  [rows]
  (let [file (first (:file rows))]
    {:file file
     :rows (into {} (for [[t rs] (dissoc rows :file :namespace) :when (seq rs)] [t (vec rs)]))}))

(defn- add-to-ix [ix fid frag]
  (reduce-kv
   (fn [ix iname [table col unique?]]
     (let [rs (if (= table :file) [(:file frag)] (get-in frag [:rows table]))]
       (if (empty? rs)
         ix
         (update ix iname
                 (fn [m]
                   (if unique?
                     (reduce (fn [m r]
                               (let [k (get r col)]
                                 (when-let [other (get m k)]
                                   (when (not= fid (or (:file-id other) (:id other)))
                                     (throw (ex-info "duplicate unique key"
                                                     {:index iname :key k}))))
                                 (assoc m k r)))
                             (or m {}) rs)
                     (reduce-kv (fn [m k rows] (assoc-in m [k fid] rows))
                                (or m {})
                                (group-by #(get % col) rs))))))))
   ix index-specs))

(defn- remove-from-ix [ix fid frag]
  (reduce-kv
   (fn [ix iname [table col unique?]]
     (let [rs (if (= table :file) [(:file frag)] (get-in frag [:rows table]))]
       (if (empty? rs)
         ix
         (update ix iname
                 (fn [m]
                   (reduce (fn [m k]
                             (if unique?
                               (dissoc m k)
                               (let [inner (dissoc (get m k) fid)]
                                 (if (empty? inner) (dissoc m k) (assoc m k inner)))))
                           m (distinct (map #(get % col) rs))))))))
   ix index-specs))

(defn- put-file [db fid rows]
  (let [frag (fragment rows)
        locals (map :id (get-in frag [:rows :local-def]))]
    (-> db
        (assoc-in [:files fid] frag)
        (update :ix add-to-ix fid frag)
        (update :next-local (fnil max 0) (reduce max 0 locals)))))

(defn- drop-file [db fid]
  (if-let [frag (get-in db [:files fid])]
    (-> db
        (update :files dissoc fid)
        (update :ix remove-from-ix fid frag))
    db))

(defn- empty-db [] {:files {} :ix {} :next-local 0})

(defn build-from-analyses
  "Index these analyses. Returns the index."
  [root entries]
  (let [analyses (analysis-maps entries)
        all-vars (dedupe-vars (mapcat :vars analyses))
        vars-by-ns (group-by :ns (vals all-vars))]
    (first
     (reduce (fn [[db next-local] [file-id a]]
               (let [r (file-rows (assoc a :project root) file-id
                                  all-vars vars-by-ns next-local)]
                 [(put-file db file-id (:rows r)) (:next-local r)]))
             [(empty-db) 0]
             (map-indexed (fn [i a] [(inc i) a]) analyses)))))

(defmacro ^:private with-lint
  "Run body with the project's lint config bound."
  [root & body]
  `(binding [*lint* (settings/lint-config ~root)] ~@body))

(defn build
  "Index every source file under root from scratch. Returns the index."
  [root]
  (with-lint root
    (let [entries (analyze-project root)]
      (remember! root entries)
      (build-from-analyses root entries))))

(defn build-cached
  "Reuse cached analyses whose content hash is unchanged, then index."
  [root]
  (with-lint root
    (let [cached (cached-entries root)
          all (timed "find files"
                     (doall (concat (map #(vector % false) (paths/source-files root))
                                    (timed "find dependency files"
                                           (doall (map #(vector % true) (paths/dep-files root)))))))
          entries
          (timed "analyze changed files"
                 (pmapv (fn [[path external]]
                          (let [source (slurp-file path)
                                h (content-hash source)
                                c (get cached path)]
                            (if (and c (= h (:hash c)) (= external (:external c)))
                              (assoc c :path path :mtime (mtime path))
                              (analysis-entry-text path source external))))
                        all))]
      (remember! root entries)
      (timed "build index" (build-from-analyses root entries)))))

(defn- patch-file
  "Replace one file's rows from its new analysis entry."
  [db root path entry]
  (let [cached (assoc (cached-entries root) path entry)
        analyses (analysis-maps (vals cached))
        all-vars (dedupe-vars (mapcat :vars analyses))
        vars-by-ns (group-by :ns (vals all-vars))
        a (some #(when (= (:file-path %) path) %) analyses)
        old (file-by-path db path)
        file-id (or (:id old) (inc (reduce max 0 (keys (:files db)))))
        fr (file-rows (assoc a :project root) file-id all-vars vars-by-ns
                      (:next-local db 0))
        db' (put-file (drop-file db file-id) file-id (:rows fr))]
    (swap! mem-cache assoc root cached)
    db'))

(defn update-file-text
  "Patch one file's rows in the index from `source`. Falls back to a cached
  rebuild when the patch cannot be applied (a duplicate definition moved
  between files, say)."
  [db root path source]
  (with-lint root
    (let [path (normalize-path path)
          external (boolean (:external (get (cached-entries root) path)))]
      (try
        (patch-file db root path (analysis-entry-text path source external))
        (catch Throwable _
          (build-cached root))))))

(defn update-file [db root path]
  (update-file-text db root path (slurp-file path)))

(defn delete-file [db root path]
  (let [path (normalize-path path)
        file (file-by-path db path)]
    (swap! mem-cache update root dissoc path)
    (if file (drop-file db (:id file)) db)))

(defn refresh [root]
  (build-cached root))

(declare all-files)

(defn stale-files
  "Project (non-dependency) files whose mtime changed since they were
  indexed, plus new and deleted ones: {:changed [...] :added [...]
  :deleted [...]}."
  [db root]
  (let [indexed (into {}
                      (keep (fn [f] (when-not (:external f) [(:path f) (:mtime f)])))
                      (all-files db))
        current (set (paths/source-files root))]
    {:changed (vec (for [[p m] indexed
                         :when (and (contains? current p) (not= m (mtime p)))]
                     p))
     :added (vec (remove #(contains? indexed %) current))
     :deleted (vec (remove current (keys indexed)))}))

;; ---------------------------------------------------------------------------
;; reads

(defn normalize-path [path]
  (let [p (fs/absolutize (str path))]
    (if (fs/exists? p)
      (str (fs/canonicalize p))
      (str (fs/normalize p)))))

(defn- multi [db iname k]
  (vec (mapcat val (get-in db [:ix iname k]))))

(defn- unique [db iname k]
  (get-in db [:ix iname k]))

(defn all-rows
  "Every row of a table, across files."
  [db table]
  (if (= table :file)
    (mapv :file (vals (:files db)))
    (vec (mapcat #(get-in % [:rows table]) (vals (:files db))))))

(defn- in-file [db table file-id]
  (or (get-in db [:files file-id :rows table]) []))

(defn file-by-path [db path]
  (unique db :file/by-path (normalize-path path)))

(defn file-by-id [db id]
  (get-in db [:files id :file]))

(defn all-files [db]
  (all-rows db :file))

(defn var-by-fqn [db fqn]
  (unique db :var-def/by-fqn fqn))

(defn vars-in-ns [db ns]
  (multi db :var-def/by-ns ns))

(defn refs-by-fqn [db fqn]
  (multi db :var-ref/by-fqn fqn))

(defn callers-of [db fqn]
  (multi db :call-edge/by-callee fqn))

(defn callees-of [db fqn]
  (multi db :call-edge/by-caller fqn))

(defn ns-deps-of [db ns]
  (multi db :ns-dep/by-from-ns ns))

(defn ns-dependents-of [db ns]
  (multi db :ns-dep/by-to-ns ns))

(defn var-defs-in-file [db file-id] (in-file db :var-def file-id))
(defn var-refs-in-file [db file-id] (in-file db :var-ref file-id))
(defn local-defs-in-file [db file-id] (in-file db :local-def file-id))
(defn local-refs-in-file [db file-id] (in-file db :local-ref file-id))
(defn keyword-defs-in-file [db file-id] (in-file db :keyword-def file-id))
(defn keyword-refs-in-file [db file-id] (in-file db :keyword-ref file-id))

(defn file-by-ns
  "The project file defining ns, preferring a project file over a
  dependency's copy."
  [db ns]
  (first (sort-by (fn [f] [(if (:external f) 1 0) (:path f)])
                  (multi db :file/by-ns ns))))

(defn- ns-rows [db table ns]
  (when-let [f (file-by-ns db ns)]
    (in-file db table (:id f))))

(defn ns-requires-of [db ns]
  (sort-by (juxt :row :col) (ns-rows db :ns-require ns)))

(defn ns-refers-of [db ns]
  (ns-rows db :ns-refer ns))

(defn ns-aliases-of [db ns]
  (ns-rows db :ns-alias ns))

(defn file-requires
  "The require rows of the file at path."
  [db path]
  (when-let [f (file-by-path db path)]
    (sort-by (juxt :row :col) (in-file db :ns-require (:id f)))))

(defn file-refers [db path]
  (when-let [f (file-by-path db path)]
    (in-file db :ns-refer (:id f))))

(defn file-aliases [db path]
  (when-let [f (file-by-path db path)]
    (in-file db :ns-alias (:id f))))

(defn all-var-defs [db]
  (vals (get-in db [:ix :var-def/by-fqn])))

(defn all-keyword-defs [db]
  (vals (get-in db [:ix :keyword-def/by-fqn])))

(defn local-def-by-id [db id]
  (unique db :local-def/by-id id))

(defn local-refs-by-local-id [db id]
  (multi db :local-ref/by-local-id id))

(defn keyword-def-by-fqn [db fqn]
  (unique db :keyword-def/by-fqn fqn))

(defn keyword-refs-by-fqn [db fqn]
  (multi db :keyword-ref/by-fqn fqn))

(defn all-keyword-refs [db]
  (all-rows db :keyword-ref))
