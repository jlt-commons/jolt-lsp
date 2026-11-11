(ns jolt-lsp.test-runner
  "Run the clojure.test namespaces in this process.

    jolt test

  Exits non-zero on any failure. Spec namespaces are checked by
  jolt-lsp.spec-runner (jolt spec), not here."
  (:require [clojure.test :as t]))

(def test-namespaces
  '[jolt-lsp.smoke-test jolt-lsp.index-test jolt-lsp.extras-test jolt-lsp.refactor-test
    jolt-lsp.syntax-test jolt-lsp.replace-test jolt-lsp.review-test
    jolt-lsp.sdiff.core-test jolt-lsp.sdiff.group-test jolt-lsp.sdiff.names-test
    jolt-lsp.sdiff.address-test jolt-lsp.surgeon.cljc-test jolt-lsp.surgeon.move-test
    jolt-lsp.surgeon.refactor-test jolt-lsp.restructure-test])

(defn -main [& _args]
  (apply require test-namespaces)
  (let [{:keys [fail error]} (apply t/run-tests test-namespaces)]
    (System/exit (if (zero? (+ (or fail 0) (or error 0))) 0 1))))
