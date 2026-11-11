(ns jolt-lsp.graph.query
  "Graph queries over the index."
  (:require [jolt-lsp.graph :as graph]
            [jolt-lsp.db :as db]))

(defn call-edges [db]
  (mapv (juxt :caller :callee) (db/all-rows db :call-edge)))

(defn ns-edges [db]
  (mapv (juxt :from-ns :to-ns) (db/all-rows db :ns-dep)))

(defn- flip [edges]
  (mapv (fn [[a b]] [b a]) edges))

(defn impact
  "Every var that transitively calls fqn."
  [db fqn]
  (disj (graph/reachable (flip (call-edges db)) fqn) fqn))

(defn impact-detail
  "Transitive callers of fqn with their distance in calls, nearest first."
  [db fqn]
  (->> (dissoc (graph/shortest-distances (flip (call-edges db)) fqn) fqn)
       (sort-by (juxt val key))
       (mapv (fn [[caller hops]] {:fqn caller :distance hops}))))

(defn call-hierarchy [db fqn direction]
  (let [edges (distinct (call-edges db))
        chosen (case direction :callers (flip edges) edges)
        nodes (disj (graph/reachable chosen fqn) fqn)]
    {:root fqn
     :direction direction
     :direct (vec (sort (graph/neighbors chosen fqn)))
     :nodes (vec (sort nodes))}))

(defn ns-dependencies [db ns]
  (disj (graph/reachable (ns-edges db) ns) ns))

(defn ns-dependents [db ns]
  (disj (graph/reachable (flip (ns-edges db)) ns) ns))

(defn ns-cycles
  "Require cycles between namespaces."
  [db]
  (mapv (comp vec sort) (graph/cycles (distinct (ns-edges db)))))
