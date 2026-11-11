(ns jolt-lsp.graph
  "Pure graph algorithms over edge lists. Breadth-first over an adjacency
  map built once per query, so every query is linear in the edges."
  (:require [clojure.set :as set]))

(defn- adjacency [edges]
  (reduce (fn [m [from to]] (update m from (fnil conj #{}) to)) {} edges))

(defn neighbors [edges node]
  (get (adjacency edges) node #{}))

(defn- bfs
  "{node distance} for every node reachable from start in adjacency map adj.
  One round per BFS layer, bounded by the node count, each round touching
  only its frontier."
  [adj start]
  (first
   (reduce (fn [[dist frontier] _]
             (if (empty? frontier)
               (reduced [dist frontier])
               (reduce (fn [[dist next] u]
                         (reduce (fn [[dist next] v]
                                   (if (contains? dist v)
                                     [dist next]
                                     [(assoc dist v (inc (get dist u))) (conj next v)]))
                                 [dist next]
                                 (get adj u)))
                       [dist []]
                       frontier)))
           [{start 0} [start]]
           (range (+ 2 (count adj))))))

(defn reachable
  "Nodes reachable from start through the directed edges, start included."
  [edges start]
  (set (keys (bfs (adjacency edges) start))))

(defn shortest-distances
  "{node hops} from start."
  [edges start]
  (bfs (adjacency edges) start))

(defn cycles
  "Strongly connected components with more than one node."
  [edges]
  (let [adj (adjacency edges)
        radj (adjacency (map (fn [[a b]] [b a]) edges))
        nodes (set (mapcat identity edges))]
    (second
     (reduce (fn [[todo out] start]
               (if-not (contains? todo start)
                 [todo out]
                 (let [scc (set/intersection (set (keys (bfs adj start)))
                                             (set (keys (bfs radj start))))]
                   [(set/difference todo scc)
                    (if (> (count scc) 1) (conj out scc) out)])))
             [nodes []]
             nodes))))
