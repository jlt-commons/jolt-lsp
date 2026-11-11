(ns jolt-lsp.test-runner
  "Run the clojure.test namespaces in this process.

    jolt test

  Exits non-zero on any failure. Spec namespaces are checked by
  jolt-lsp.spec-runner (jolt spec), not here."
  (:require [clojure.test :as t]))

(def test-namespaces
  '[jolt-lsp.smoke-test jolt-lsp.index-test jolt-lsp.extras-test jolt-lsp.refactor-test])

(defn -main [& _args]
  (apply require test-namespaces)
  (let [{:keys [fail error]} (apply t/run-tests test-namespaces)]
    (System/exit (if (zero? (+ (or fail 0) (or error 0))) 0 1))))
