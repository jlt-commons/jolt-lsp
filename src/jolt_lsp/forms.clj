(ns jolt-lsp.forms
  "Structural form queries and formatting-preserving edits."
  (:require [jolt-lsp.cursor :as cursor]
            [jolt-lsp.read-impl :as read]
            [jolt-lsp.text :as text]
            [jolt-lsp.tree :as tree]))

(defn top-level-forms [source]
  (let [parsed (read/parse-text source)]
    (mapv (fn [n]
            {:text (tree/text n)
             :range (tree/node-range n)
             :node n})
          (:forms parsed))))

(defn form-at [source row col]
  (when-let [chain (seq (cursor/enclosing-forms source row col))]
    (let [n (first chain)]
      {:text (tree/text n)
       :range (tree/node-range n)
       :depth (count chain)})))

(defn enclosing-form [source row col]
  (when-let [chain (seq (cursor/enclosing-forms source row col))]
    (let [n (last chain)]
      {:text (tree/text n)
       :range (tree/node-range n)})))

(defn- offset [source [row col]]
  (text/position->offset source [row col]))

(defn edit-for-range [source [sr sc er ec] new-text]
  (let [start (offset source [sr sc])
        end (offset source [er ec])]
    {:start-offset start
     :end-offset end
     :start-row sr :start-col sc :end-row er :end-col ec
     :start-byte (text/utf8-offset source start)
     :end-byte (text/utf8-offset source end)
     :old-text (subs source start end)
     :new-text new-text}))

(defn apply-edits [source edits]
  (let [sorted (sort-by :start-offset > edits)]
    (reduce (fn [s e]
              (str (subs s 0 (:start-offset e))
                   (:new-text e)
                   (subs s (:end-offset e))))
            source
            sorted)))

(defn form-sexpr [node]
  (tree/sexpr node))

(defn find-form
  "The top-level form whose sexpr equals `old-form`, or the first enclosing
  form at a position."
  [source old-form]
  (let [parsed (read/parse-text old-form)
        target (some-> (:forms parsed) first form-sexpr)]
    (some (fn [f]
            (when (= target (form-sexpr (:node f))) f))
          (top-level-forms source))))

(defn replace-form [source old-form new-text]
  (when-let [f (find-form source old-form)]
    (edit-for-range source (:range f) new-text)))

(defn insert-after [source anchor-form new-text]
  (when-let [f (find-form source anchor-form)]
    (let [r (:range f)
          end [ (nth r 2) (nth r 3) ]]
      {:start-offset (offset source end)
       :end-offset (offset source end)
       :start-row (nth r 2) :start-col (nth r 3)
       :end-row (nth r 2) :end-col (nth r 3)
       :old-text ""
       :new-text (str "\n" new-text)})))

(defn ns-form [source]
  (let [parsed (read/parse-text source)]
    (some (fn [n]
            (when (and (= :list (tree/tag n))
                       (= "ns" (some-> (first (tree/children n)) tree/text)))
              n))
          (:forms parsed))))

(defn ns-require-clause [source]
  (when-let [ns (ns-form source)]
    (some (fn [n]
            (when (and (= :list (tree/tag n))
                       (= ":require" (some-> (first (tree/children n)) tree/text)))
              n))
          (rest (tree/children ns)))))

(defn ns-libspecs [source]
  (when-let [clause (ns-require-clause source)]
    (vec (rest (tree/children clause)))))
