(ns jolt-lsp.lsp-version-test
  "publishDiagnostics carries the version of the document it was computed
  for, so a client can match each push to its text."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [jolt-lsp.api :as api]
            [jolt-lsp.lsp :as lsp]
            [jolt-lsp.lsp.rpc :as rpc]
            [jolt-lsp.paths :as paths]))

(defn- run
  "Send `msgs` through the LSP handler. Returns the publishDiagnostics params
  pushed after each message, as a vector of vectors."
  [msgs]
  (let [root (str (fs/canonicalize (fs/create-temp-dir {:prefix "jolt-lsp-version"})))
        path (str root "/src/a.clj")
        handle @#'lsp/handle]
    (fs/create-dirs (str root "/src"))
    (spit (str root "/deps.edn") "{:paths [\"src\"]}")
    (spit path "(ns a)\n")
    (api/ensure-db root)
    ;; an initialized server, as after the initialize handshake
    (reset! lsp/state {:docs {} :versions {} :initialized? true})
    (let [pushed (atom [])]
      (with-redefs [rpc/notify (fn [method params]
                                 (when (= "textDocument/publishDiagnostics" method)
                                   (swap! pushed conj params)))]
        (vec (for [mk msgs]
               (do (reset! pushed [])
                   (handle (mk (paths/file-uri path)))
                   @pushed)))))))

(defn- msg [method params] {:jsonrpc "2.0" :method method :params params})

(deftest version-rides-with-each-push
  (let [[open change save save-again]
        (run [(fn [uri] (msg "textDocument/didOpen"
                             {:textDocument {:uri uri :languageId "clojure" :version 3 :text "(ns a)\n(def x 1)\n"}}))
              (fn [uri] (msg "textDocument/didChange"
                             {:textDocument {:uri uri :version 4}
                              :contentChanges [{:text "(ns a)\n(def x 2)\n"}]}))
              (fn [uri] (msg "textDocument/didSave" {:textDocument {:uri uri}}))
              (fn [uri] (msg "textDocument/didSave" {:textDocument {:uri uri}}))])]
    (testing "didOpen"
      (is (= [3] (mapv :version open))))
    (testing "didChange"
      (is (= [4] (mapv :version change))))
    (testing "didSave sends the last stored version"
      (is (= [4] (mapv :version save))))
    (testing "a second didSave still sends it"
      (is (= [4] (mapv :version save-again))))))

(deftest no-version-for-unopened-uri
  (let [[pushed] (run [(fn [uri] (msg "textDocument/didSave" {:textDocument {:uri uri}}))])]
    (is (= 1 (count pushed)))
    (is (not (contains? (first pushed) :version)))))

(deftest close-drops-the-version
  (run [(fn [uri] (msg "textDocument/didOpen"
                       {:textDocument {:uri uri :languageId "clojure" :version 7 :text "(ns a)\n"}}))])
  (let [[path] (keys (:versions @lsp/state))]
    (is (= 7 (get-in @lsp/state [:versions path])))
    (@#'lsp/handle (msg "textDocument/didClose" {:textDocument {:uri (paths/file-uri path)}}))
    (is (not (contains? (:versions @lsp/state) path)))))
