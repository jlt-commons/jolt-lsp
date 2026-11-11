(ns jolt-lsp.graph
  "Pure graph algorithms over edge lists. Fixed-point iteration, no recursion."
  (:require [clojure.set :as set]))

(defn- adjacency [edges]
  (reduce (fn [m [from to]] (update m from (fnil conj #{}) to)) {} edges))

(defn neighbors [edges node]
  (get (adjacency edges) node #{}))

(defn reachable
  "Nodes reachable from start through the directed edges."
  [edges start]
  (let [rounds (inc (count edges))]
    (reduce (fn [seen _]
              (into seen (mapcat #(neighbors edges %) seen)))
            #{start}
            (range rounds))))

(defn shortest-distances [edges start]
  (let [rounds (inc (count edges))]
    (reduce
     (fn [dist _]
       (reduce (fn [d [u v]]
                 (if (and (contains? d u)
                          (not (contains? d v)))
                   (assoc d v (inc (get d u)))
                   d))
               dist
               edges))
     {start 0}
     (range rounds))))

(defn cycles
  "Strongly connected components with more than one node."
  [edges]
  (let [nodes (set (mapcat identity edges))
        rev (mapv (fn [[a b]] [b a]) edges)]
    (->> nodes
         (keep (fn [start]
                 (let [reach (reachable edges start)
                       back (reachable rev start)
                       scc (set/intersection reach back)]
                   (when (> (count scc) 1) scc))))
         distinct
         vec)))
