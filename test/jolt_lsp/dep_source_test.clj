(ns jolt-lsp.dep-source-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [babashka.fs :as fs]
            [jolt-lsp.api :as api]
            [jolt-lsp.dep-source :as ds]
            [jolt-lsp.mcp :as mcp]))

(def root (str (fs/canonicalize "test/fixtures/deps-src")))

(defn- libs [r opts] (set (map :lib (:libraries (api/deps-list r opts)))))

(deftest lists-the-projects-deps
  (is (= #{"acme/widget"} (libs root {})))
  (testing "an alias adds its extra deps"
    (is (= #{"acme/widget" "acme/gadget"} (libs root {:aliases ["extra"]}))))
  (testing "pattern filters"
    (is (= #{} (libs root {:pattern "gadget"})))))

(defn- engines []
  (cond-> [false] (ds/rg-available?) (conj true)))

(deftest grep-searches-only-dependency-sources
  (doseq [rg (engines)]
    (binding [ds/*rg* rg]
      (testing (str "rg " rg)
        (let [r (api/deps-grep root {:pattern "defn spin"})
              files (set (map :file (:results r)))]
          (is (= [{:lib "acme/widget" :line 4 :text "(defn spin"}]
                 (mapv #(select-keys % [:lib :line :text]) (:results r))))
          (is (every? #(string/ends-with? % "lib/widget/src/acme/widget.clj") files))
          (is (not (:truncated r))))
        (testing "the project's own sources and unrelated dirs are not searched"
          (is (empty? (:results (api/deps-grep root {:pattern "spin-secret|defn run"})))))
        (testing "case-insensitive"
          (is (= #{4 9} (set (map :line (:results (api/deps-grep root {:pattern "defn spin"
                                                                      :case-insensitive true}))))))
          (is (= #{4} (set (map :line (:results (api/deps-grep root {:pattern "defn spin"})))))))
        (testing "glob"
          (is (= ["README.md"]
                 (mapv :path (:results (api/deps-grep root {:pattern "spin the widget"
                                                            :glob "*.md"})))))
          (is (empty? (:results (api/deps-grep root {:pattern "spin the widget"
                                                     :glob "*.clj"})))))
        (testing "library filter and aliases"
          (is (empty? (:results (api/deps-grep root {:pattern "spin-gadget"}))))
          (is (= ["acme/gadget"]
                 (mapv :lib (:results (api/deps-grep root {:pattern "spin-gadget"
                                                           :aliases ["extra"]})))))
          (is (empty? (:results (api/deps-grep root {:pattern "Spin n times"
                                                     :library "gadget"
                                                     :aliases ["extra"]}))))
          (is (= ["acme/widget"]
                 (mapv :lib (:results (api/deps-grep root {:pattern "Spin n times"
                                                           :library "widget"
                                                           :aliases ["extra"]})))))
          (is (false? (:ok (api/deps-grep root {:pattern "x" :library "nope"})))))
        (testing "limit"
          (let [r (api/deps-grep root {:pattern "spin" :limit 2})]
            (is (= 2 (count (:results r))))
            (is (:truncated r))))
        (testing "files only"
          (is (= #{"README.md" "src/acme/widget.clj"}
                 (set (map :path (:results (api/deps-grep root {:pattern "spin"
                                                                :files-only true})))))))))))

(deftest grep-needs-a-pattern
  (is (false? (:ok (api/deps-grep root {})))))

(deftest read-a-dependency-file
  (let [file (:file (first (:results (api/deps-grep root {:pattern "defn spin"}))))]
    (testing "by the path grep returned"
      (let [r (api/deps-read root {:file file :offset 4 :limit 2})]
        (is (= "acme/widget" (:lib r)))
        (is (= 4 (:start r)))
        (is (= 5 (:end r)))
        (is (:truncated r))
        (is (string/includes? (:content r) "(defn spin"))
        (is (not (string/includes? (:content r) "[n]")))))
    (testing "by library and path"
      (let [r (api/deps-read root {:library "acme/widget" :path "src/acme/widget.clj"})]
        (is (= file (:file r)))
        (is (= 9 (:total-lines r)))))))

(deftest read-refuses-paths-outside-the-deps
  (doseq [opts [{:file (str root "/outside/secret.clj")}
                {:file (str root "/src/app/main.clj")}
                {:file (str root "/lib/widget/../../outside/secret.clj")}
                {:library "acme/widget" :path "../../outside/secret.clj"}
                {:library "acme/widget" :path "/etc/hosts"}
                {:file (str root "/lib/gadget/src/acme/gadget.clj")}]]
    (is (false? (:ok (api/deps-read root opts))) (pr-str opts)))
  (is (true? (:ok (api/deps-read root {:file (str root "/lib/gadget/src/acme/gadget.clj")
                                       :aliases ["extra"]})))))

(deftest jar-dependencies-are-searched-and-read
  (let [tmp (str (fs/create-temp-dir))
        src (str tmp "/jarsrc")]
    (fs/copy-tree "test/fixtures/deps-src" (str tmp "/p"))
    (fs/create-dirs (str src "/acme"))
    (spit (str src "/acme/jarred.clj") "(ns acme.jarred)\n\n(defn jarred-fn [] 1)\n")
    (fs/zip (str tmp "/jarred.jar") [(str src "/acme")] {:root src})
    (spit (str tmp "/p/deps.edn")
          (pr-str {:paths ["src"]
                   :deps {'acme/jarred {:local/root (str tmp "/jarred.jar")}}}))
    (let [p (str (fs/canonicalize (str tmp "/p")))
          hits (:results (binding [ds/*rg* false]
                           (api/deps-grep p {:pattern "defn jarred-fn"})))]
      (is (= [["acme/jarred" "acme/jarred.clj" 3]]
             (mapv (juxt :lib :path :line) hits)))
      (is (string/includes? (:content (api/deps-read p {:file (:file (first hits))}))
                            "jarred-fn")))))

(deftest mcp-lists-the-tools
  (let [names (set (map :name (get-in (mcp/handle {:jsonrpc "2.0" :id 1 :method "tools/list"})
                                      [:result :tools])))]
    (is (every? names ["jolt_deps_list" "jolt_deps_grep" "jolt_deps_read"])))
  (let [resp (mcp/handle {:jsonrpc "2.0" :id 2 :method "tools/call"
                          :params {:name "jolt_batch"
                                   :arguments {:root root
                                               :ops [{:tool "jolt_deps_grep"
                                                      :args {:pattern "defn spin"}}]}}})]
    (is (string/includes? (get-in resp [:result :content 0 :text]) "widget.clj"))))

(deftest read-decides-containment-on-the-real-path
  ;; a path that reaches a dependency through a symlink is that dependency's
  ;; file; one that leaves through a symlink is refused (tested above)
  (let [tmp (str (fs/create-temp-dir {:prefix "deps-link"}))
        link (str tmp "/w")]
    (try
      (fs/create-sym-link link (str root "/lib/widget"))
      (let [r (api/deps-read root {:file (str link "/src/acme/widget.clj")})]
        (is (:ok r) (:error r))
        (is (= "acme/widget" (:lib r))))
      (finally (fs/delete-tree tmp)))))

(deftest rg-keeps-matches-when-some-files-are-unreadable
  (when (ds/rg-available?)
    (let [f (str root "/lib/widget/src/acme/locked.clj")]
      (try
        (spit f "(ns acme.locked)\n")
        (fs/set-posix-file-permissions f "---------")
        (binding [ds/*rg* true]
          (let [r (api/deps-grep root {:pattern "defn spin"})]
            (is (nil? (:error r)) (:error r))
            (is (= [4] (mapv :line (:results r))))))
        (finally (fs/delete-if-exists f))))))
