(ns jolt-lsp.mcp-root-test
  "Which project an MCP call without root works on."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [jolt-lsp.mcp :as mcp]))

(defn- temp-project [prefix]
  (let [d (str (fs/canonicalize (fs/create-temp-dir {:prefix prefix})))]
    (spit (str d "/deps.edn") "{:paths [\"src\"]}")
    (fs/create-dirs (str d "/src/app"))
    (spit (str d "/src/app/core.clj") "(ns app.core)\n(defn greet [n] (str \"hi \" n))\n")
    d))

(deftest default-root-order
  (let [proj (temp-project "jl-proj")
        other (temp-project "jl-other")
        plugin (str (fs/canonicalize (fs/create-temp-dir {:prefix "jl-plugin"})))]
    (try
      (spit (str plugin "/kimi.plugin.json") "{}")
      (spit (str plugin "/deps.edn") "{}")
      (testing "an explicit root wins"
        (is (= other (mcp/default-root {:root other :file (str proj "/src/app/core.clj")
                                        :last proj :user-dir proj}))))
      (testing "then the project holding an absolute file"
        (is (= proj (mcp/default-root {:file (str proj "/src/app/core.clj")
                                       :last other :user-dir other}))))
      (testing "then the last root used"
        (is (= other (mcp/default-root {:file "src/app/core.clj" :last other :user-dir proj}))))
      (testing "then the server's directory"
        (is (= proj (mcp/default-root {:user-dir proj}))))
      (testing "but never a plugin's install directory"
        (is (nil? (mcp/default-root {:user-dir plugin})))
        (is (nil? (mcp/default-root {:last plugin}))))
      (finally
        (doseq [d [proj other plugin]] (fs/delete-tree d {:force true}))))))

(deftest a-call-from-a-plugin-dir-without-root-asks-for-one
  (let [plugin (str (fs/canonicalize (fs/create-temp-dir {:prefix "jl-plugin"})))
        proj (temp-project "jl-proj")]
    (try
      (spit (str plugin "/kimi.plugin.json") "{}")
      (with-redefs [mcp/server-dir (constantly plugin)
                    mcp/last-root (constantly nil)]
        (let [r (mcp/call-tool "jolt_index" {})]
          (is (false? (:ok r)))
          (is (re-find #"pass root" (:error r))))
        (testing "an absolute file finds its project"
          (let [r (mcp/call-tool "jolt_symbols" {"file" (str proj "/src/app/core.clj")})]
            (is (not (false? (:ok r))) (pr-str r))
            (is (re-find #"greet" (pr-str r))))))
      (finally
        (doseq [d [proj plugin]] (fs/delete-tree d {:force true}))))))
