(ns jolt-lsp.edit.rename
  "Rename a var, local, or keyword. Returns a preview; writing is opt-in."
  (:require [clojure.string :as string]
            [jolt-lsp.cursor :as cursor]
            [jolt-lsp.db :as db]
            [jolt-lsp.edit.io :as edit-io]
            [jolt-lsp.edit.vedit :as vedit]
            [jolt-lsp.query :as query]))

(defn- row-at [rows row col]
  (some (fn [r]
          (when (query/pos-in? row col [(:row r) (:col r) (:end-row r) (:end-col r)])
            r))
        rows))

(defn- name-at [rows row col]
  (some (fn [r]
          (when (query/pos-in? row col [(:name-row r) (:name-col r)
                                        (:name-end-row r) (:name-end-col r)])
            r))
        rows))

(defn target-at [db path row col]
  (let [file (db/file-by-path db path)
        fid (:id file)
        text (slurp path)
        token (cursor/symbol-at text row col)]
    (cond
      (:keyword token)
      (let [kr (row-at (db/keyword-refs-in-file db fid) row col)
            kd (row-at (db/keyword-defs-in-file db fid) row col)]
        (cond
          kr {:kind :keyword :fqn (:fqn kr) :file-id fid}
          kd {:kind :keyword :fqn (:fqn kd) :file-id fid}
          :else nil))

      (:symbol token)
      (let [lr (row-at (db/local-refs-in-file db fid) row col)
            ld (row-at (db/local-defs-in-file db fid) row col)
            vr (row-at (db/var-refs-in-file db fid) row col)
            vd (name-at (db/var-defs-in-file db fid) row col)]
        (cond
          lr {:kind :local :local-id (:local-id lr) :file-id fid}
          ld {:kind :local :local-id (:id ld) :file-id fid}
          vr (when (:fqn vr) {:kind :var :fqn (:fqn vr)})
          vd {:kind :var :fqn (:fqn vd)}
          :else nil))

      :else nil)))

(defn- kw-name [fqn]
  (let [s (if (string/starts-with? fqn "::")
            (subs fqn 2)
            (if (string/starts-with? fqn ":") (subs fqn 1) fqn))]
    (if-let [i (string/last-index-of s "/")]
      (subs s (inc i))
      s)))

(defn- kw-range [row name]
  (let [r [(:row row) (:col row) (:end-row row) (:end-col row)]]
    [(first r) (- (nth r 3) (count name)) (nth r 2) (nth r 3)]))

(defn- sites [db target]
  (case (:kind target)
    :var
    (let [fqn (:fqn target)
          v (db/var-by-fqn db fqn)]
      (concat
       (when v [{:file-id (:file-id v)
                 :range [(:name-row v) (:name-col v) (:name-end-row v) (:name-end-col v)]}])
       (map (fn [r] {:file-id (:file-id r)
                     :range [(:name-row r) (:name-col r) (:name-end-row r) (:name-end-col r)]})
            (db/refs-by-fqn db fqn))))

    :local
    (let [lid (:local-id target)
          l (db/local-def-by-id db lid)]
      (concat
       (when l [{:file-id (:file-id l)
                 :range [(:row l) (:col l) (:end-row l) (:end-col l)]}])
       (map (fn [r] {:file-id (:file-id r)
                     :range [(:row r) (:col r) (:end-row r) (:end-col r)]})
            (db/local-refs-by-local-id db lid))))

    :keyword
    (let [fqn (:fqn target)
          nm (kw-name fqn)
          kd (db/keyword-def-by-fqn db fqn)]
      (concat
       (when kd [{:file-id (:file-id kd) :range (kw-range kd nm)}])
       (map (fn [r] {:file-id (:file-id r) :range (kw-range r nm)})
            (db/keyword-refs-by-fqn db fqn))))

    []))

(defn- edits-by-file [db target to]
  (->> (sites db target)
       (group-by :file-id)
       (reduce-kv
        (fn [m fid xs]
          (let [path (:path (db/file-by-id db fid))
                source (slurp path)]
            (assoc m path (mapv #(vedit/range->edit source (:range %) to) xs))))
        {})))

(defn rename [db {:keys [path row col to]}]
  (let [target (target-at db path row col)]
    (cond
      (nil? target) {:ok false :reason "no renameable symbol at position"}
      (not (string? to)) {:ok false :reason "new name must be a string"}
      :else
      (let [edits (edits-by-file db target to)
            files (reduce-kv (fn [m p es]
                               (assoc m p (vedit/apply-edits (slurp p) es)))
                             {} edits)]
        {:ok true
         :target target
         :to to
         :edits edits
         :preview files}))))

(defn apply! [result]
  (when (:ok result)
    (edit-io/write-files! {:files (:preview result)})))
