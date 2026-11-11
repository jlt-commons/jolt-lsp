(ns jolt-lsp.query
  "Read-side engine queries over the relatum index."
  (:require [clojure.string :as string]
            [jolt-lsp.cursor :as cursor]
            [jolt-lsp.db :as db]
            [jolt-lsp.diagnostics :as diagnostics]
            [relatum.core :as rc]))

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

(defn definition-at
  "Definition for the symbol at a 1-based row/col in `path`."
  [db path row col]
  (let [file (db/file-by-path db path)
        fid (:id file)
        text (try (slurp path) (catch Throwable _ ""))
        token (cursor/symbol-at text row col)]
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

      :else [])))

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

(defn hover
  [db fqn]
  (when-let [v (db/var-by-fqn db fqn)]
    (assoc (var-location db v)
           :doc (:doc v)
           :arglists (:arglists v)
           :private (:private v)
           :defined-by (:defined-by v))))

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
