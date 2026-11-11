(ns jolt-lsp.query
  "Read-side engine queries over the index."
  (:require [clojure.string :as string]
            [jolt-lsp.cursor :as cursor]
            [jolt-lsp.db :as db]
            [jolt-lsp.diagnostics :as diagnostics]))

(defn pos-in? [row col r]
  (when r
    (let [[sr sc er ec] r]
      (and (or (> row sr) (and (= row sr) (>= col sc)))
           (or (< row er) (and (= row er) (< col ec)))))))

(defn- file-path [db file-id]
  (:path (db/file-by-id db file-id)))

(defn var-location [db v]
  (when v
    {:path (file-path db (:file-id v))
     :fqn (:fqn v)
     :name (:name v)
     :kind (:kind v)
     :range [(:name-row v) (:name-col v) (:name-end-row v) (:name-end-col v)]
     :row (:row v) :col (:col v)
     :end-row (:end-row v) :end-col (:end-col v)}))

(defn ref-location [db r]
  (when r
    {:path (file-path db (:file-id r))
     :fqn (:fqn r)
     :name (:name r)
     :range [(:name-row r) (:name-col r) (:name-end-row r) (:name-end-col r)]
     :row (:row r) :col (:col r)
     :end-row (:end-row r) :end-col (:end-col r)}))

(defn local-location [db l]
  (when l
    {:path (file-path db (:file-id l))
     :name (:name l)
     :kind (:kind l)
     :range [(:row l) (:col l) (:end-row l) (:end-col l)]}))

(defn keyword-location [db k]
  (when k
    {:path (file-path db (:file-id k))
     :fqn (:fqn k)
     :kind (:kind k)
     :range [(:row k) (:col k) (:end-row k) (:end-col k)]}))

(defn- row-at [rows row col]
  (some (fn [r]
          (when (pos-in? row col [(:row r) (:col r) (:end-row r) (:end-col r)])
            r))
        rows))

(defn- name-at [rows row col]
  (some (fn [r]
          (when (pos-in? row col [(:name-row r) (:name-col r)
                                  (:name-end-row r) (:name-end-col r)])
            r))
        rows))

(defn token-at
  "The symbol or keyword at row/col, or just before it (a cursor sitting
  right after a name, as editors and agents often place it)."
  [text row col]
  (or (cursor/symbol-at text row col)
      (when (> col 1)
        (when-let [t (cursor/symbol-at text row (dec col))]
          (assoc t :col-adjusted true)))))

(defn definition-at
  "Definition for the symbol at a 1-based row/col in `path`. `source` is
  the file's current text when it is not what is on disk."
  ([db path row col] (definition-at db path row col nil))
  ([db path row col source]
  (let [file (db/file-by-path db path)
        fid (:id file)
        text (or source (try (slurp (:path file path)) (catch Throwable _ "")))
        token (token-at text row col)
        col (if (:col-adjusted token) (dec col) col)]
    (cond
      (:keyword token)
      (let [kr (row-at (db/keyword-refs-in-file db fid) row col)
            kd (if kr
                 (db/all-keyword-defs db)
                 (db/keyword-defs-in-file db fid))
            hit (if kr
                  (some #(when (= (:fqn %) (:fqn kr)) %) kd)
                  (row-at kd row col))]
        (if hit [(keyword-location db hit)] []))

      (:symbol token)
      (or
       (when-let [lr (row-at (db/local-refs-in-file db fid) row col)]
         (some->> (db/local-defs-in-file db fid)
                  (some #(when (= (:id %) (:local-id lr)) %))
                  (local-location db)
                  vector))
       (when-let [ld (row-at (db/local-defs-in-file db fid) row col)]
         [(local-location db ld)])
       (when-let [vr (row-at (db/var-refs-in-file db fid) row col)]
         (when-let [v (db/var-by-fqn db (:fqn vr))]
           [(var-location db v)]))
       (when-let [vd (name-at (db/var-defs-in-file db fid) row col)]
         [(var-location db vd)])
       [])

      :else []))))

(defn references
  "Every site that names `fqn`, plus its definition."
  [db fqn]
  (let [vars (when-let [v (db/var-by-fqn db fqn)] [(var-location db v)])
        refs (mapv #(ref-location db %) (db/refs-by-fqn db fqn))]
    (vec (concat vars (sort-by (juxt :path :row :col) refs)))))

(defn- subsequence? [q n]
  (loop [cs (seq q)
         s (seq n)]
    (cond
      (empty? cs) true
      (empty? s) false
      (= (first cs) (first s)) (recur (rest cs) (rest s))
      :else (recur cs (rest s)))))

(defn score [name query]
  (let [n (string/lower-case name)
        q (string/lower-case query)]
    (cond
      (= n q) 0
      (string/starts-with? n q) 1
      (string/includes? n q) 2
      (subsequence? q n) 3
      :else nil)))

(defn search
  [db query limit]
  (->> (db/all-var-defs db)
       (keep (fn [v]
               (when-let [s (score (:name v) query)]
                 (assoc (var-location db v) :score s :arglists (:arglists v) :doc (:doc v)))))
       (sort-by (juxt :score :fqn))
       (take (or limit 50))
       vec))

(def ^:private max-source-lines 80)

(defn source-text
  "The text of a var's whole definition form, capped at
  max-source-lines lines."
  [db v]
  (try
    (let [text (slurp (file-path db (:file-id v)))
          lines (string/split text #"\n" -1)
          from (dec (:row v))
          to (min (:end-row v) (+ from max-source-lines))
          body (subvec lines from to)
          body (assoc body 0 (subs (first body) (min (count (first body)) (dec (:col v)))))
          last-i (dec (count body))
          body (if (= to (:end-row v))
                 (update body last-i #(subs % 0 (min (count %)
                                                    (if (zero? last-i)
                                                      (- (:end-col v) (:col v))
                                                      (dec (:end-col v))))))
                 (conj body (str ";; ... " (- (:end-row v) to) " more lines")))]
      (string/join "\n" body))
    (catch Throwable _ nil)))

(defn hover
  [db fqn]
  (when-let [v (db/var-by-fqn db fqn)]
    (assoc (var-location db v)
           :doc (:doc v)
           :arglists (:arglists v)
           :private (:private v)
           :defined-by (:defined-by v)
           :code (source-text db v))))

(defn document-symbols [db path]
  (when-let [file (db/file-by-path db path)]
    (->> (db/var-defs-in-file db (:id file))
         (sort-by (juxt :name-row :name-col))
         (mapv (fn [v]
                 {:name (:name v)
                  :fqn (:fqn v)
                  :kind (:kind v)
                  :range [(:row v) (:col v) (:end-row v) (:end-col v)]
                  :selection-range [(:name-row v) (:name-col v)
                                    (:name-end-row v) (:name-end-col v)]
                  :detail (when (:private v) "private")})))))

(defn context [db ns]
  (let [vars (sort-by :name (db/vars-in-ns db ns))
        file (db/file-by-ns db ns)
        deps (db/ns-deps-of db ns)
        dependents (db/ns-dependents-of db ns)]
    {:ns ns
     :file (when file (:path file))
     :vars (mapv #(select-keys % [:fqn :name :kind :arglists :doc :private])
                 vars)
     :dependencies (mapv :to-ns deps)
     :dependents (mapv :from-ns dependents)
     :diagnostics (when file (diagnostics/file-diagnostics db (:path file)))}))

(defn file-ns-summary
  "Requires, aliases and refers of the file at path, for an agent about to
  edit it."
  [db path]
  (when-let [f (db/file-by-path db path)]
    {:ns (:ns f)
     :requires (mapv #(cond-> {:ns (:target %)}
                        (:alias %) (assoc :alias (:alias %))
                        (:refer-all %) (assoc :refer :all))
                     (db/file-requires db path))
     :refers (into (sorted-map) (map (juxt :name :target)) (db/file-refers db path))}))
