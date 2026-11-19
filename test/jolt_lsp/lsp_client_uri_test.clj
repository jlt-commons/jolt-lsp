(ns jolt-lsp.lsp-client-uri-test
  "A document opened through a symlink gets its diagnostics under the URI
  the client opened it with, not the canonical path the index uses."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [jolt-lsp.lsp :as lsp]
            [jolt-lsp.lsp.rpc :as rpc]
            [jolt-lsp.paths :as paths]))

(def ^:private source "(ns demo.core)\n(defn total [items] (sum-prices items))\n")

(defn- project
  "A project in a real directory, reached through a symlink beside it.
  Returns {:real real-root :link link-root}."
  []
  (let [base (str (fs/canonicalize (fs/create-temp-dir {:prefix "jolt-lsp-uri"})))
        real (str base "/real")
        link (str base "/link")]
    (fs/create-dirs (str real "/src/demo"))
    (spit (str real "/deps.edn") "{:paths [\"src\"]}")
    (spit (str real "/src/demo/core.clj") source)
    (fs/create-sym-link link real)
    {:real real :link link}))

(defn- req [id method params] {:jsonrpc "2.0" :id id :method method :params params})
(defn- note [method params] {:jsonrpc "2.0" :method method :params params})
(defn- open [uri] (note "textDocument/didOpen"
                        {:textDocument {:uri uri :languageId "clojure" :version 1 :text source}}))
(defn- close [uri] (note "textDocument/didClose" {:textDocument {:uri uri}}))
(defn- save [uri] (note "textDocument/didSave" {:textDocument {:uri uri}}))

(defn- run
  "Initialize a fresh server at `root-uri`, then send `msgs`. Returns the
  URIs of the publishDiagnostics pushes after each message."
  [root-uri msgs]
  (let [sent (atom [])]
    (reset! lsp/state {:docs {} :versions {}})
    (with-redefs [rpc/write-message #(swap! sent conj %)]
      (@#'lsp/handle (req 1 "initialize" {:rootUri root-uri :capabilities {}}))
      (vec (for [m msgs]
             (do (reset! sent [])
                 (@#'lsp/handle m)
                 (->> @sent
                      (filter #(= "textDocument/publishDiagnostics" (:method %)))
                      (mapv #(get-in % [:params :uri])))))))))

(deftest a-push-uses-the-uri-the-client-opened
  (let [{:keys [real link]} (project)
        link-uri (paths/file-uri (str link "/src/demo/core.clj"))
        real-uri (paths/file-uri (str real "/src/demo/core.clj"))
        [opened saved] (run (paths/file-uri link) [(open link-uri) (save link-uri)])]
    (is (not= link-uri real-uri))
    (testing "didOpen through the link pushes to the link URI"
      (is (= [link-uri] opened)))
    (testing "and so does a later didSave"
      (is (= [link-uri] saved)))))

(deftest locations-in-an-open-document-use-its-uri
  (let [{:keys [link]} (project)
        link-uri (paths/file-uri (str link "/src/demo/core.clj"))
        sent (atom [])]
    (run (paths/file-uri link) [(open link-uri)])
    (with-redefs [rpc/write-message #(swap! sent conj %)]
      ;; `total` in (defn total ...), line 1
      (@#'lsp/handle (req 2 "textDocument/references"
                          {:textDocument {:uri link-uri} :position {:line 1 :character 7}
                           :context {:includeDeclaration true}})))
    (let [locs (:result (first @sent))]
      (is (seq locs))
      (is (= #{link-uri} (set (map :uri locs)))))))

(deftest a-file-the-client-never-opened-is-named-under-the-clients-root
  (let [{:keys [real link]} (project)]
    (testing "a root reached through a link names unopened files through it"
      (let [[saved] (run (paths/file-uri link)
                         [(save (paths/file-uri (str link "/src/demo/core.clj")))])]
        (is (= [(paths/file-uri (str link "/src/demo/core.clj"))] saved))))
    (testing "a canonical root keeps canonical URIs"
      (let [[saved] (run (paths/file-uri real)
                         [(save (paths/file-uri (str real "/src/demo/core.clj")))])]
        (is (= [(paths/file-uri (str real "/src/demo/core.clj"))] saved))))))

(deftest navigation-into-an-unopened-file-uses-the-clients-root
  (let [{:keys [real link]} (project)
        other (str real "/src/demo/util.clj")
        _ (spit other "(ns demo.util)\n(defn sum-prices [xs] (reduce + xs))\n")
        _ (spit (str real "/src/demo/core.clj")
                "(ns demo.core (:require [demo.util :refer [sum-prices]]))\n(defn total [items] (sum-prices items))\n")
        link-uri (paths/file-uri (str link "/src/demo/core.clj"))
        sent (atom [])]
    (run (paths/file-uri link) [])
    (with-redefs [rpc/write-message #(swap! sent conj %)]
      (@#'lsp/handle (req 2 "textDocument/definition"
                          {:textDocument {:uri link-uri} :position {:line 1 :character 22}})))
    (is (= [(paths/file-uri (str link "/src/demo/util.clj"))]
           (mapv :uri (:result (first @sent)))))))

(deftest uris-are-percent-encoded
  (testing "a path with a space or a non-ASCII name round trips"
    (doseq [p ["/tmp/my project/src/a.clj" "/tmp/café/a.clj" "/tmp/a+b/c#d.clj"]]
      (is (= p (paths/uri->path (paths/file-uri p))))))
  (testing "the URI itself is encoded as clients send it"
    (is (= "file:///tmp/my%20project/a.clj" (paths/file-uri "/tmp/my project/a.clj")))
    (is (= "file:///tmp/caf%C3%A9/a.clj" (paths/file-uri "/tmp/café/a.clj"))))
  (testing "a client's encoded URI decodes"
    (is (= "/tmp/a b/x.clj" (paths/uri->path "file:///tmp/a%20b/x.clj")))
    (is (= "/tmp/a+b/x.clj" (paths/uri->path "file:///tmp/a+b/x.clj")))))

(deftest one-file-open-under-two-uris-gets-a-push-for-each
  (let [{:keys [real link]} (project)
        link-uri (paths/file-uri (str link "/src/demo/core.clj"))
        real-uri (paths/file-uri (str real "/src/demo/core.clj"))
        [_ both _ after-close] (run (paths/file-uri real)
                                    [(open link-uri) (open real-uri)
                                     (close link-uri) (save real-uri)])]
    (is (= #{link-uri real-uri} (set both)))
    (is (= 2 (count both)))
    (testing "closing one leaves the other, and keeps the open buffer"
      (is (= [real-uri] after-close))
      (is (= [source] (vals (:docs @lsp/state)))))))

(deftest closing-the-last-uri-forgets-it
  (let [{:keys [real link]} (project)
        link-uri (paths/file-uri (str link "/src/demo/core.clj"))
        ;; rooted at the real path, so after the close nothing maps it to the link
        [_ _ saved] (run (paths/file-uri real) [(open link-uri) (close link-uri) (save link-uri)])]
    (is (= [(paths/file-uri (str real "/src/demo/core.clj"))] saved))
    (is (empty? (:uris @lsp/state)))))
