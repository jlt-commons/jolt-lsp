(ns jolt-lsp.smoke-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [jolt-lsp.cli :as cli]
            [jolt-lsp.version :as version]))

(deftest version-is-a-string
  (is (string? version/version))
  (is (seq version/version)))

(deftest help-names-the-project
  (testing "help loads and names the usage line"
    (is (string/includes? (with-out-str (cli/run ["--help"]))
                          "usage: jolt-lsp"))))
