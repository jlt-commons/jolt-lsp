(ns jolt-lsp.edit.rename
  "Rename a var, local, or keyword. Returns a preview; writing is opt-in.

  `sources` maps a path to its current text when that is not the file on
  disk (an editor buffer). Definitions in dependencies are refused: the
  edit would land in a jar's extracted copy."
  (:require [clojure.string :as string]
            [jolt-lsp.cursor :as cursor]
            [jolt-lsp.db :as db]
            [jolt-lsp.edit.io :as edit-io]
            [jolt-lsp.edit.vedit :as vedit]
            [jolt-lsp.query :as query]))

(defn- source-of [sources path]
  (or (get sources path) (slurp path)))

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

(defn target-at
  ([db path row col] (target-at db path row col nil))
  ([db path row col sources]
   (let [file (db/file-by-path db path)
         fid (:id file)
         text (source-of sources (:path file path))
         token (cursor/symbol-at text row col)]
     (cond
       (nil? file) nil

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

       :else nil))))

(defn- kw-name [fqn]
  (let [s (string/replace fqn #"^::?" "")]
    (if-let [i (string/last-index-of s "/")]
      (subs s (inc i))
      s)))

(defn- kw-range
  "The name part at the end of a keyword or :keys-entry occurrence."
  [row name]
  [(:end-row row) (- (:end-col row) (count name)) (:end-row row) (:end-col row)])

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
          kd (db/keyword-def-by-fqn db fqn)
          refs (db/keyword-refs-by-fqn db fqn)
          range-of (fn [r] [(:row r) (:col r) (:end-row r) (:end-col r)])
          ;; {:keys [a]} binds a local named after the keyword: its uses
          ;; follow the rename
          destructured (for [[fid rs] (group-by :file-id refs)
                             :let [at (set (map range-of rs))]
                             l (db/local-defs-in-file db fid)
                             :when (and (:keys-destructuring l) (at (range-of l)))
                             lr (db/local-refs-by-local-id db (:id l))]
                         {:file-id fid :range (range-of lr)})]
      (concat
       (when kd [{:file-id (:file-id kd) :range (kw-range kd nm)}])
       (map (fn [r] {:file-id (:file-id r) :range (kw-range r nm)}) refs)
       destructured))

    []))

(defn- new-name
  "The text that replaces each site: the bare name, whatever was passed."
  [target to]
  (let [t (string/trim (str to))]
    (if (= :keyword (:kind target))
      (kw-name t)
      (if-let [i (string/index-of t "/")] (subs t (inc i)) t))))

(defn- valid-name? [s]
  (and (seq s)
       (re-matches #"[^\s,;()\[\]{}\"'`~@^\\:#0-9][^\s,;()\[\]{}\"`~@^\\]*" s)
       (not (string/ends-with? s ":"))))

(defn rename
  "opts: :path :row :col :to, and :sources {path text} for unsaved text."
  [db {:keys [path row col to sources]}]
  (let [target (target-at db path row col sources)
        nm (when target (new-name target to))]
    (cond
      (nil? target) {:ok false :reason "no renameable symbol at position"}
      (not (string? to)) {:ok false :reason "new name must be a string"}
      (not (valid-name? nm)) {:ok false :reason (str "not a valid name: " (pr-str nm))}
      :else
      (let [by-file (group-by :file-id (distinct (sites db target)))
            files (into {} (map (fn [fid] [fid (db/file-by-id db fid)])) (keys by-file))]
        (if-let [ext (first (filter :external (vals files)))]
          {:ok false
           :reason (str "the definition is in a dependency (" (:path ext) "); rename it there")}
          (let [edits (into {}
                            (for [[fid xs] by-file
                                  :let [path (:path (get files fid))
                                        source (source-of sources path)]]
                              [path (mapv #(vedit/range->edit source (:range %) nm) xs)]))
                previews (into {}
                               (for [[p es] edits]
                                 [p (vedit/apply-edits (source-of sources p) es)]))]
            {:ok true
             :target target
             :to nm
             :edits edits
             :preview previews}))))))

(defn apply! [result]
  (when (:ok result)
    (edit-io/write-files! {:files (:preview result)})))
