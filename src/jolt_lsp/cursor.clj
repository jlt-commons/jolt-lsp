(ns jolt-lsp.cursor
  "Find the node at a 1-based row/col. Positions are internal (codepoints);
  callers convert from LSP or CLI coordinates first."
  (:require [jolt-lsp.read-impl :as read]
            [jolt-lsp.tree :as tree]))

(defn in-range? [[r c er ec] row col]
  (and (or (> row r) (and (= row r) (>= col c)))
       (or (< row er) (and (= row er) (< col ec)))))

(defn- walk [node row col]
  (let [r (tree/node-range node)]
    (when (in-range? r row col)
      (or (when (tree/branch? node)
            (some #(walk % row col) (tree/children node)))
          {:node node :range r}))))

(defn node-at [text row col]
  (let [parsed (read/parse-text text)]
    (some #(walk % row col) (:forms parsed))))

(defn symbol-at [text row col]
  (when-let [hit (node-at text row col)]
    (let [n (:node hit)]
      (when (or (tree/symbol-node? n) (tree/keyword-node? n))
        {:node n
         :range (:range hit)
         :text (tree/text n)
         :symbol (tree/symbol-node? n)
         :keyword (tree/keyword-node? n)}))))

(defn enclosing-forms [text row col]
  (let [parsed (read/parse-text text)]
    (letfn [(chain [node]
              (when (in-range? (tree/node-range node) row col)
                (cons node (when (tree/branch? node)
                             (some chain (tree/children node))))))]
      (reverse (some chain (:forms parsed))))))
