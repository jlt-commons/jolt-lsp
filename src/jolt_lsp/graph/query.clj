(ns jolt-lsp.graph.query
  "Graph queries over the relatum index."
  (:require [jolt-lsp.db :as db]
            [jolt-lsp.graph :as graph]
            [relatum.core :as rc]))

(defn call-edges [db]
  (mapv (juxt :caller :callee) (rc/rows db :call-edge)))

(defn ns-edges [db]
  (mapv (juxt :from-ns :to-ns) (rc/rows db :ns-dep)))

(defn impact [db fqn]
  (let [reverse-edges (mapv (fn [[a b]] [b a]) (call-edges db))]
    (disj (graph/reachable reverse-edges fqn) fqn)))

(defn call-hierarchy [db fqn direction]
  (let [edges (call-edges db)
        chosen (case direction :callers (mapv (fn [[a b]] [b a]) edges) edges)
        nodes (disj (graph/reachable chosen fqn) fqn)]
    {:root fqn
     :edges (vec (filter (fn [[a b]] (or (= a fqn) (= b fqn)))
                         (if (= direction :callers) chosen edges)))
     :nodes (vec (sort nodes))}))

(defn ns-dependencies [db ns]
  (disj (graph/reachable (ns-edges db) ns) ns))

(defn ns-dependents [db ns]
  (let [rev (mapv (fn [[a b]] [b a]) (ns-edges db))]
    (disj (graph/reachable rev ns) ns)))
