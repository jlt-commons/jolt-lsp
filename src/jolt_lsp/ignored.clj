(ns jolt-lsp.ignored
  "Ranges of #_ discard forms and (comment ...) blocks."
  (:require [jolt-lsp.read-impl :as read]
            [jolt-lsp.tree :as tree]))

(defn ranges [source]
  (let [parsed (read/parse-text source)]
    (letfn [(walk [node acc]
              (let [tag (tree/tag node)
                    txt (when (tree/branch? node) nil)]
                (cond
                  (= :uneval tag) (conj acc (tree/node-range node))
                  (and (= :list tag)
                       (= "comment" (some-> (first (tree/children node)) tree/text)))
                  (conj acc (tree/node-range node))
                  (tree/branch? node) (reduce (fn [a c] (walk c a)) acc (tree/children node))
                  :else acc)))]
      (reduce (fn [acc n] (walk n acc)) [] (:forms parsed)))))
