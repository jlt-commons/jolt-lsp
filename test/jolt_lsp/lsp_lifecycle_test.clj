(ns jolt-lsp.lsp-lifecycle-test
  "Requests before initialize are refused, and the watched-files
  registration is sent only to a client that supports it."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [jolt-lsp.lsp :as lsp]
            [jolt-lsp.lsp.rpc :as rpc]
            [jolt-lsp.paths :as paths]))

(defn- root-dir []
  (let [root (str (fs/canonicalize (fs/create-temp-dir {:prefix "jolt-lsp-lifecycle"})))]
    (fs/create-dirs (str root "/src"))
    (spit (str root "/deps.edn") "{:paths [\"src\"]}")
    (spit (str root "/src/a.clj") "(ns a)\n")
    root))

(defn- run
  "Send `msgs` through a fresh server's handler. Returns what it wrote back,
  as a vector of JSON-RPC messages."
  [msgs]
  (let [sent (atom [])]
    (reset! lsp/state {:docs {} :versions {}})
    (with-redefs [rpc/write-message #(swap! sent conj %)]
      (doseq [m msgs] (@#'lsp/handle m)))
    @sent))

(defn- req [id method params] {:jsonrpc "2.0" :id id :method method :params params})
(defn- note [method params] {:jsonrpc "2.0" :method method :params params})

(defn- initialize [root caps]
  (req 1 "initialize" {:rootUri (paths/file-uri root) :capabilities caps}))

(deftest requests-before-initialize-are-refused
  (let [root (root-dir)
        uri (paths/file-uri (str root "/src/a.clj"))
        [hover & more] (run [(req 7 "textDocument/hover"
                                  {:textDocument {:uri uri} :position {:line 0 :character 1}})
                             (note "textDocument/didOpen"
                                   {:textDocument {:uri uri :version 1 :text "(ns a)\n"}})])]
    (testing "a request gets ServerNotInitialized"
      (is (= 7 (:id hover)))
      (is (= -32002 (get-in hover [:error :code]))))
    (testing "a notification is dropped, so nothing is pushed for it"
      (is (empty? more))
      (is (empty? (:docs @lsp/state))))))

(deftest initialize-opens-the-server
  (let [root (root-dir)
        uri (paths/file-uri (str root "/src/a.clj"))
        [init hover] (run [(initialize root {})
                           (req 2 "textDocument/hover"
                                {:textDocument {:uri uri} :position {:line 0 :character 1}})])]
    (is (contains? (:result init) :capabilities))
    (is (= 2 (:id hover)))
    (is (not (contains? hover :error)))))

(defn- registrations [sent]
  (filter #(= "client/registerCapability" (:method %)) sent))

(deftest watched-files-registration-needs-client-support
  (let [root (root-dir)
        supports {:workspace {:didChangeWatchedFiles {:dynamicRegistration true}}}]
    (testing "a client that supports dynamic registration gets it"
      (let [regs (registrations (run [(initialize root supports) (note "initialized" {})]))]
        (is (= ["workspace/didChangeWatchedFiles"]
               (mapv #(get-in % [:params :registrations 0 :method]) regs)))))
    (testing "a client that says no, or says nothing, does not"
      (is (empty? (registrations
                   (run [(initialize root {:workspace {:didChangeWatchedFiles {:dynamicRegistration false}}})
                         (note "initialized" {})]))))
      (is (empty? (registrations (run [(initialize root {}) (note "initialized" {})])))))))
