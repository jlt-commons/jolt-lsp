(ns jolt-lsp.extras-test
  (:require [clojure.test :refer [deftest is]]
            [babashka.fs :as fs]
            [jolt-lsp.api :as api]
            [jolt-lsp.db :as db]
            [jolt-lsp.format]
            [jolt-lsp.generate :as generate]
            [jolt-lsp.ignored :as ignored]
            [jolt-lsp.mcp :as mcp]
            [jolt-lsp.watch :as watch]))

(def root (str (babashka.fs/canonicalize "test/fixtures/simple")))

(deftest ns-from-path
  (is (= "foo.bar-baz" (generate/ns-for-path root (str root "/src/foo/bar_baz.clj"))))
  (is (= "(ns foo.bar-baz)\n" (generate/ns-form "foo.bar-baz"))))

(deftest ignored-form-ranges
  (let [rs (ignored/ranges "(ns a)\n#_(def x 1)\n(comment (def y 2))\n")]
    (is (= 2 (count rs)))))

(deftest cache-roundtrip
  (let [d1 (db/build-cached root)
        d2 (db/build-cached root)]
    (is (= (count (db/all-files d1)) (count (db/all-files d2))))))

(deftest watch-snapshot
  (is (>= (count (watch/snapshot root)) 6)))

(deftest mcp-tools-and-call
  (let [list-resp (mcp/handle {:jsonrpc "2.0" :id 1 :method "tools/list"})
        call-resp (mcp/handle {:jsonrpc "2.0" :id 2 :method "tools/call"
                               :params {:name "jolt_search"
                                        :arguments {:root root :query "greet"}}})]
    (is (seq (get-in list-resp [:result :tools])))
    (is (string? (get-in call-resp [:result :content 0 :text])))))

(deftest api-definition
  (let [locs (api/definition root {:file (str root "/src/app/core.clj") :line 10 :col 16})]
    (is (= "app.util/trimmed" (:fqn (first locs))))))


(deftest incremental-update-and-delete
  (let [tmp (str (fs/create-temp-dir))
        _ (fs/copy-tree "test/fixtures/simple" tmp)
        core (str tmp "/src/app/core.clj")
        d (db/build-cached tmp)
        before (count (db/all-files d))
        d2 (db/update-file-text d tmp core
                                (str (slurp core) "\n(defn brand-new [] 1)\n"))]
    (is (some? (db/var-by-fqn d2 "app.core/brand-new")))
    (is (= before (count (db/all-files d2))))
    (let [d3 (db/update-file-text d2 tmp core "(ns app.core)\n")]
      (is (nil? (db/var-by-fqn d3 "app.core/brand-new")))
      (is (nil? (db/var-by-fqn d3 "app.core/greet")))
      (let [d4 (db/delete-file d3 tmp core)]
        (is (= (dec before) (count (db/all-files d4))))))))

(deftest formatting-preview
  (let [tmp (str (fs/create-temp-dir))
        f (str tmp "/x.clj")]
    (spit f "(defn f[x](+ x 1))")
    (let [r (jolt-lsp.format/format-file tmp f)]
      (is (:ok r))
      (is (:changed r))
      (is (= "(defn f [x] (+ x 1))" (:preview r))))))

(deftest custom-and-layer-linters
  (let [tmp (str (fs/create-temp-dir))
        _ (fs/copy-tree "test/fixtures/simple" tmp)
        core (str tmp "/src/app/core.clj")
        _ (fs/create-dirs (str tmp "/linters"))
        _ (fs/create-dirs (str tmp "/.jolt-lsp"))
        _ (spit (str tmp "/linters/my.clj")
                "(ns my.linters)\n(defn analyze [ctx] [{:row 1 :col 1 :end-row 1 :end-col 2 :severity :warning :code :my-warning :message \"custom\"}])\n")
        _ (spit (str tmp "/.jolt-lsp/config.edn")
                (pr-str {:kondo false
                         :custom-linters [{:file "linters/my.clj" :fn "my.linters/analyze"}]
                         :clj-depend {:rules [{:from "app.core" :to "app.util"}]}}))
        _ (api/refresh! tmp)
        ds (api/diagnostics* tmp {:file core})]
    (is (some #(and (= :custom (:source %)) (= :my-warning (:code %))) ds))
    (is (some #(= :layer-violation (:code %)) ds))))
