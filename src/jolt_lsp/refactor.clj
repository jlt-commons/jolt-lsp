(ns jolt-lsp.refactor
  "Small structural refactors over the index: sort requires, cycle a keyword,
  inline a single-use let binding, destructure a map binding, move a form."
  (:require [clojure.string :as string]
            [jolt-lsp.cursor :as cursor]
            [jolt-lsp.db :as db]
            [jolt-lsp.edit.vedit :as vedit]
            [jolt-lsp.forms :as forms]
            [jolt-lsp.read-impl :as read]
            [jolt-lsp.text :as text]
            [jolt-lsp.tree :as tree]))

(defn- preview [edits-by-file]
  (let [files (reduce-kv (fn [m p es]
                           (assoc m p (vedit/apply-edits (slurp p) es)))
                         {} edits-by-file)]
    {:ok true
     :changed (boolean (seq edits-by-file))
     :edits edits-by-file
     :preview files}))

(defn- single [path edits]
  (preview {path edits}))

(defn- distinct-by [f xs]
  (vals (reduce (fn [m x] (assoc m (f x) x)) {} xs)))

(defn- node-range= [a b] (= a b))

(defn sort-requires [db path]
  (let [source (slurp path)
        clause (forms/ns-require-clause source)
        specs (forms/ns-libspecs source)]
    (if (and clause (seq specs))
      (let [sorted (sort-by tree/text specs)
            indent (apply str (repeat (+ 2 (nth (tree/node-range clause) 1)) " "))
            text (str "(:require\n" (string/join "\n" (map #(str indent (tree/text %)) sorted)) ")")
            edit (vedit/range->edit source (tree/node-range clause) text)]
        (single path [edit]))
      {:ok true :changed false :reason "no require clause"})))

(defn- keyword-at [db path row col]
  (let [file (db/file-by-path db path)
        fid (:id file)]
    (or (some #(when (and (= (:row %) row) (<= (:col %) col) (<= col (:end-col %))) %)
              (db/keyword-refs-in-file db fid))
        (some #(when (and (= (:row %) row) (<= (:col %) col) (<= col (:end-col %))) %)
              (db/keyword-defs-in-file db fid)))))

(defn cycle-keyword [db path row col]
  (let [k (keyword-at db path row col)
        source (slurp path)]
    (if (nil? k)
      {:ok false :reason "no keyword at position"}
      (let [fqn (:fqn k)
            ns (:ns (db/file-by-path db path))
            aliases (into {} (map (juxt :target :alias)) (or (db/ns-aliases-of db ns) []))
            notation (:notation k)
            new-text (cond
                       (= notation :auto) fqn
                       (= notation :auto-alias) fqn
                       (= notation :qualified)
                       (let [q (subs fqn 1)
                             i (string/index-of q "/")
                             prefix (when i (subs q 0 i))
                             name (if i (subs q (inc i)) q)]
                         (cond
                           (= prefix ns) (str "::" name)
                           (contains? aliases prefix) (str "::" (get aliases prefix) "/" name)
                           :else nil))
                       :else nil)]
        (if new-text
          (single path [(vedit/range->edit source [(:row k) (:col k)
                                                    (:end-row k) (:end-col k)] new-text)])
          {:ok false :reason "cannot cycle this keyword"})))))

(defn- local-at [db path row col]
  (let [file (db/file-by-path db path)
        fid (:id file)
        ref-hit (some #(when (and (= (:row %) row)
                                  (<= (:col %) col)
                                  (<= col (:end-col %)))
                         (:local-id %))
                      (db/local-refs-in-file db fid))]
    (if ref-hit
      (db/local-def-by-id db ref-hit)
      (some #(when (and (= (:row %) row) (<= (:col %) col) (<= col (:end-col %))) %)
            (db/local-defs-in-file db fid)))))

(defn- find-vector [source row col]
  (some #(when (= :vector (tree/tag %)) %)
        (reverse (cursor/enclosing-forms source row col))))

(defn inline-symbol [db path row col]
  (let [l (local-at db path row col)
        source (slurp path)]
    (if-not l
      {:ok false :reason "no local at position"}
      (let [refs (db/local-refs-by-local-id db (:id l))]
        (if (not= 1 (count refs))
          {:ok false :reason "local is not used exactly once"}
          (let [v (find-vector source (:row l) (:col l))
                cs (when v (tree/children v))
                idx (when (seq cs)
                      (first (keep-indexed (fn [i n]
                                             (when (node-range= (tree/node-range n)
                                                                [(:row l) (:col l) (:end-row l) (:end-col l)])
                                               i))
                                           cs)))
                pat (when (and idx (< (inc idx) (count cs))) (nth cs idx))
                init (when pat (nth cs (inc idx)))
                ref (first refs)]
            (if (and pat init ref)
              (let [binding-edit (vedit/range->edit
                                  source
                                  (let [a (tree/node-range pat) b (tree/node-range init)]
                                    [(first a) (nth a 1) (nth b 2) (nth b 3)])
                                  "")
                    ref-edit (vedit/range->edit source
                                                [(:row ref) (:col ref)
                                                 (:end-row ref) (:end-col ref)]
                                                (tree/text init))]
                (single path [binding-edit ref-edit]))
              {:ok false :reason "could not find the let binding"})))))))

(defn destructure-keys [db path row col]
  (let [l (local-at db path row col)
        source (slurp path)]
    (if-not l
      {:ok false :reason "no local at position"}
      (let [refs (db/local-refs-by-local-id db (:id l))
            info (keep (fn [r]
                         (let [chain (cursor/enclosing-forms source (:row r) (:col r))
                               call (some (fn [n]
                                            (when (= :list (tree/tag n))
                                              (let [cs (tree/children n)
                                                    k (first cs)
                                                    target (second cs)]
                                                (when (and (tree/keyword-node? k)
                                                           (= (:name l) (tree/text target)))
                                                  {:key (subs (tree/text k) 1)
                                                   :range (tree/node-range n)}))))
                                          chain)]
                           call))
                       refs)]
        (if (or (empty? refs) (not= (count info) (count refs)))
          {:ok false :reason "every use must be (:key local)"}
          (let [keys (sort (distinct (map :key info)))
                binding-edit (vedit/range->edit source
                                                [(:row l) (:col l) (:end-row l) (:end-col l)]
                                                (str "{:keys [" (string/join " " keys) "]}"))
                ref-edits (mapv (fn [{:keys [key range]}]
                                  (vedit/range->edit source range key))
                                (distinct-by :key info))]
            (single path (into [binding-edit] ref-edits))))))))

(defn move-form [db path row col target-file]
  (let [source (slurp path)
        forms (forms/top-level-forms source)
        f (some #(when (and (<= (nth (:range %) 0) row)
                            (<= row (nth (:range %) 2)))
                   %) forms)]
    (if-not (and f target-file)
      {:ok false :reason "no top-level form at position"}
      (let [delete (vedit/range->edit source (:range f) "")
            target-old (slurp target-file)
            append {:start-offset (count target-old) :end-offset (count target-old)
                    :start-row 1 :start-col 1 :end-row 1 :end-col 1
                    :start-byte (text/utf8-offset target-old (count target-old))
                    :end-byte (text/utf8-offset target-old (count target-old))
                    :old-text ""
                    :new-text (str "\n\n" (:text f))}]
        (preview {path [delete] target-file [append]})))))
