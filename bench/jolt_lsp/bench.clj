(ns jolt-lsp.bench
  "Index benchmarks.

    jolt bench [root]

  Prints cold and warm index times and query latency."
  (:require [jolt-lsp.db :as db]
            [jolt-lsp.graph.query :as graph]
            [jolt-lsp.query :as query]))

(defn- ms [f]
  (let [t0 (System/nanoTime)
        v (f)]
    [(quot (- (System/nanoTime) t0) 1000000) v]))

(defn -main [& args]
  (let [root (or (first args) "test/fixtures/simple")]
    (println "root:" root)
    (let [[cold d] (ms #(db/build root))
          [warm _] (ms #(db/build-cached root))
          vars (db/all-var-defs d)
          fqn (:fqn (first vars))
          [def-ms _] (ms #(when fqn (query/search d (name fqn) 10)))
          [refs-ms _] (ms #(when fqn (query/references d fqn)))
          [imp-ms _] (ms #(when fqn (graph/impact d fqn)))]
      (println "files:" (count (db/all-files d)))
      (println "vars:" (count vars))
      (println "cold index ms:" cold)
      (println "warm index ms:" warm)
      (println "search ms:" def-ms)
      (println "references ms:" refs-ms)
      (println "impact ms:" imp-ms)))
  (System/exit 0))
