(ns jolt-lsp.spec.graph-spec
  "Pure graph laws: reachability, distances, and cycles over edge lists."
  (:require [writ.spec :refer [spec ann graph law]]))

(spec jolt-lsp.graph {:require :tested})

(ann neighbors [(List (Tuple String String)) String -> (Set String)])
(ann reachable [(List (Tuple String String)) String -> (Set String)])
(ann shortest-distances [(List (Tuple String String)) String -> (Map String Nat)])
(ann cycles [(List (Tuple String String)) -> (List (Set String))])

(graph traversing
  {:start :edges
   :states {:edges (List (Tuple String String))
            :nodes (Set String)
            :dists (Map String Nat)
            :sccs (List (Set String))}
   :edges {:edges {[neighbors String] #{:nodes}
                   [reachable String] #{:nodes}
                   [shortest-distances String] #{:dists}
                   [cycles] #{:sccs}}}
   :final [:nodes :dists :sccs]})

(law reachable-has-start
  (forall [e (List (Tuple String String)), s String]
    (contains? (reachable e s) s)))

(law direct-edge-reachable
  (forall [a String, b String]
    (contains? (reachable [[a b]] a) b)))

(law neighbors-direct
  (forall [a String, b String]
    (contains? (neighbors [[a b]] a) b)))

(law no-edges-no-neighbors
  (forall [a String]
    (empty? (neighbors [] a))))

(law shortest-start
  (forall [e (List (Tuple String String)), s String]
    (= 0 (get (shortest-distances e s) s))))

(law shortest-direct
  (forall [a String, b String]
    (=> (not= a b)
        (= 1 (get (shortest-distances [[a b]] a) b)))))

(law cycle-found
  (forall [a String, b String]
    (=> (not= a b)
        (some (fn [c] (and (contains? c a) (contains? c b)))
              (cycles [[a b] [b a]])))))

(law dag-has-no-cycle
  (forall [a String, b String]
    (empty? (cycles [[a b]]))))

(law no-edges-only-start
  (forall [s String]
    (= #{s} (reachable [] s))))
