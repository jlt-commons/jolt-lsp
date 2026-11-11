(ns jolt-lsp.edit.clean-ns
  "Drop unused and duplicate requires and unused refers, preserving
  everything else.

  Follows clojure-lsp's clean-ns: only vector libspecs are removed (a bare
  `app.routes` is a side-effect load), a libspec goes with the whitespace
  that separated it from its neighbour, and a :require clause left empty
  goes too. Libspecs inside reader conditionals or prefix lists are left
  alone."
  (:require [jolt-lsp.db :as db]
            [jolt-lsp.diagnostics :as diagnostics]
            [jolt-lsp.edit.vedit :as vedit]
            [jolt-lsp.forms :as forms]
            [jolt-lsp.tree :as tree]))

(defn- blank? [ch]
  (or (= ch \,) (Character/isWhitespace ch)))

(defn removal-span
  "[start end) codepoint offsets that remove `child` from `parent` cleanly:
  the child and the blanks before it, or, for the first element of a
  vector or the first after a list's head, the blanks after it (so the
  next element takes its place)."
  [source parent child]
  (let [kids (tree/children parent)
        idx (first (keep-indexed (fn [i n] (when (identical? n child) i)) kids))
        start (:start child)
        end (:end child)
        ws-start (loop [i start] (if (and (pos? i) (blank? (nth source (dec i)))) (recur (dec i)) i))
        ws-end (loop [i end] (if (and (< i (count source)) (blank? (nth source i))) (recur (inc i)) i))
        first-idx (if (= :vector (tree/tag parent)) 0 1)]
    (if (and (= idx first-idx) (< (inc idx) (count kids)))
      [start ws-end]
      [ws-start end])))

(defn- removal-edit [source parent child]
  (let [[s e] (removal-span source parent child)]
    (vedit/offsets->edit source s e "")))

(defn- range-of [row]
  [(:row row) (:col row) (:end-row row) (:end-col row)])

(defn- keep-duplicate?
  "Of several requires of one namespace, keep the ones whose alias or
  refers are used, else the first."
  [u refers group]
  (let [used (filter #(diagnostics/require-used? u refers (assoc % :bare false)) group)
        keepers (set (if (seq used) used [(first group)]))]
    (fn [row] (contains? keepers row))))

(defn plan
  "Edits that clean the ns form of the file at path. `source` defaults to
  the file on disk."
  ([db path] (plan db path nil))
  ([db path source]
   (when-let [file (db/file-by-path db path)]
     (let [source (or source (slurp path))
           fid (:id file)
           refs (db/var-refs-in-file db fid)
           u (#'diagnostics/usage refs (db/keyword-refs-in-file db fid))
           requires (db/file-requires db path)
           refers (db/file-refers db path)
           dupes (->> (group-by :target requires)
                      (vals)
                      (filter #(> (count %) 1))
                      (mapcat (fn [g] (remove (keep-duplicate? u refers g) g)))
                      set)
           unused (set (concat dupes
                               (remove #(diagnostics/require-used? u refers %) requires)))
           unused-ranges (set (map range-of unused))
           unused-refer-ranges (->> (filter :refer refs)
                                    (remove #(contains? (:refer-uses u) [(:fqn %) (:text %)]))
                                    (map range-of)
                                    set)
           {ns-node :ns clause :clause libspecs :libspecs} (forms/ns-parts source)
           doomed (filter #(and (= :vector (tree/tag (tree/meta-form %)))
                                (contains? unused-ranges (tree/node-range (tree/meta-form %))))
                          libspecs)
           doomed-set (set doomed)
           refer-edits
           (for [ls libspecs
                 :when (and (not (doomed-set ls)) (= :vector (tree/tag (tree/meta-form ls))))
                 :let [lsv (tree/meta-form ls)
                       [_ & opts] (tree/children lsv)
                       pairs (partition-all 2 opts)
                       [k v] (first (filter #(= ":refer" (tree/text (first %))) pairs))]
                 :when (and v (= :vector (tree/tag v)))
                 :let [names (tree/children v)
                       gone (filter #(contains? unused-refer-ranges
                                                (tree/node-range (tree/meta-form %)))
                                    names)]
                 :when (seq gone)
                 e (if (= (count gone) (count names))
                     ;; every refer unused: drop `:refer [...]` itself
                     (let [[ks _] (removal-span source lsv k)
                           [_ ve] (removal-span source lsv v)]
                       [(vedit/offsets->edit source ks ve "")])
                     (map #(removal-edit source v %) gone))]
             e)
           lib-edits (cond
                       (empty? doomed) []
                       (= (count doomed) (count libspecs))
                       [(removal-edit source ns-node clause)]
                       :else (map #(removal-edit source clause %) doomed))
           edits (vec (concat lib-edits refer-edits))]
       {:ok true
        :path path
        :changed (boolean (seq edits))
        :edits edits
        :preview (vedit/apply-edits source edits)
        :removed (vec (concat (map #(tree/text (first (tree/children (tree/meta-form %)))) doomed)
                              (->> (filter :refer refs)
                                   (filter #(contains? unused-refer-ranges (range-of %)))
                                   (map :fqn))))}))))

(defn apply! [result]
  (when (and (:ok result) (:changed result))
    (spit (:path result) (:preview result))))
