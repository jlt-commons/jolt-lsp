(ns jolt-lsp.refactor
  "Small structural refactors over the index: sort requires, cycle a keyword,
  inline a single-use let binding, destructure a map binding, move a form."
  (:require [clojure.string :as string]
            [jolt-lsp.cursor :as cursor]
            [jolt-lsp.db :as db]
            [jolt-lsp.edit.vedit :as vedit]
            [jolt-lsp.forms :as forms]
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

(defn- node-range= [a b] (= a b))

(defn sort-requires
  "Sort the libspecs of the :require clause, keeping its layout: the first
  libspec stays where it was and the rest line up under it. A clause with
  comments is left for a human, since a comment's libspec is ambiguous."
  [_db path]
  (let [source (slurp path)
        {:keys [clause libspecs]} (forms/ns-parts source)]
    (cond
      (not (and clause (seq libspecs)))
      {:ok true :changed false :reason "no require clause"}

      (some #(= :comment (tree/tag %)) (tree/all-children clause))
      {:ok false :reason "the require clause has comments; sort it by hand"}

      :else
      (let [sorted (sort-by #(string/replace (tree/text %) #"^[\[(]" "") libspecs)
            [r1 c1] (tree/node-range (first libspecs))
            [_ _ er ec] (tree/node-range (last libspecs))
            indent (apply str (repeat (dec c1) " "))
            text (string/join (str "\n" indent) (map tree/text sorted))]
        (if (= sorted libspecs)
          {:ok true :changed false :reason "already sorted"}
          (single path [(vedit/range->edit source [r1 c1 er ec] text)]))))))

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
    (cond
      (not l)
      {:ok false :reason "no local at position"}

      (not (#{:let} (:kind l)))
      {:ok false :reason (str "only let bindings can be inlined; this is a "
                              (name (or (:kind l) :binding)))}

      :else
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
                pat (when (and idx (even? idx) (< (inc idx) (count cs))) (nth cs idx))
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
                                                (when (and (= 2 (count cs))
                                                           (tree/keyword-node? k)
                                                           (not (string/starts-with? (tree/text k) "::"))
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
                                  (vedit/range->edit source range
                                                     (last (string/split key #"/"))))
                                info)]
            (single path (into [binding-edit] ref-edits))))))))

(defn- binding-vector-at [source row col]
  (some (fn [n]
          (when (= :list (tree/tag n))
            (let [cs (tree/children n)
                  head (some-> (first cs) tree/text)
                  v (second cs)]
              (when (and (contains? #{"let" "let*" "loop" "loop*" "when-let" "if-let"
                                     "when-some" "if-some" "with-open" "doseq" "for"}
                                   head)
                         v (= :vector (tree/tag v)))
                v))))
        (reverse (cursor/enclosing-forms source row col))))

(defn- map-keys [node]
  (when (= :map (tree/tag node))
    (let [cs (tree/children node)]
      (mapcat (fn [[k v]]
                (when (and (= ":keys" (tree/text k)) (= :vector (tree/tag v)))
                  (keep (fn [n] (let [t (tree/text n)]
                                  (when-not (string/includes? t "/") t)))
                        (tree/children v))))
              (partition-all 2 cs)))))

(defn- pair-info [pair]
  (let [pat (first pair)
        init (second pair)]
    (when (and pat init)
      (let [range [(first (tree/node-range pat)) (nth (tree/node-range pat) 1)
                   (nth (tree/node-range init) 2) (nth (tree/node-range init) 3)]]
        (cond
          (and (tree/symbol-node? pat)
               (= :list (tree/tag init))
               (= 2 (count (tree/children init)))
               (tree/keyword-node? (first (tree/children init)))
               (tree/symbol-node? (second (tree/children init))))
          (let [k (first (tree/children init))
                m (second (tree/children init))
                kt (tree/text k)]
            (when (and (string/starts-with? kt ":") (not (string/includes? (subs kt 1) "/")))
              {:kind :call :range range :map-sym (tree/text m)
               :keys [(subs kt 1)]}))

          (and (= :map (tree/tag pat))
               (tree/symbol-node? init))
          (let [ks (vec (map-keys pat))]
            (when (seq ks)
              {:kind :destructure :range range :map-sym (tree/text init)
               :keys ks}))

          :else nil)))))

(defn restructure-keys [_db path row col]
  (let [source (slurp path)
        v (binding-vector-at source row col)
        cs (when v (tree/children v))
        infos (keep pair-info (partition-all 2 cs))
        groups (group-by :map-sym infos)]
    (if (empty? groups)
      {:ok false :reason "no restructurable bindings at position"}
      (let [edits
            (mapcat (fn [[_ infos]]
                      (let [ks (sort (distinct (mapcat :keys infos)))
                            first-info (first infos)
                            rest-infos (rest infos)]
                        (concat
                         [(vedit/range->edit source (:range first-info)
                                             (str "{:keys [" (string/join " " ks) "]} "
                                                  (:map-sym first-info)))]
                         (map (fn [i] (vedit/range->edit source (:range i) ""))
                              rest-infos))))
                    groups)]
        (single path (vec edits))))))

(defn move-form [_db path row _col target-file]
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
        (assoc (preview {path [delete] target-file [append]})
               :warning "references to the moved form, and the requires it needs, are not updated")))))
