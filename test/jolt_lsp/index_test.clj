(ns jolt-lsp.index-test
  (:require [clojure.test :refer [deftest is]]
            [babashka.fs :as fs]
            [jolt-lsp.db :as db]
            [jolt-lsp.diagnostics :as diagnostics]
            [jolt-lsp.edit.add-require :as add-require]
            [jolt-lsp.edit.clean-ns :as clean-ns]
            [jolt-lsp.edit.rename :as rename]
            [jolt-lsp.forms :as forms]
            [jolt-lsp.graph.query :as graph]
            [jolt-lsp.query :as query]
            [jolt-lsp.read :as read]
            [jolt-lsp.text :as text]))

(def root (str (fs/canonicalize "test/fixtures/simple")))
(def core (str root "/src/app/core.clj"))
(def util (str root "/src/app/util.clj"))

(defonce index (delay (db/build root)))

(deftest build-index
  (let [d @index]
    (is (>= (count (db/all-files d)) 6))
    (is (some? (db/file-by-path d core)))
    (is (some? (db/file-by-path d (str root "/resources/config.edn"))))))

(deftest definition-and-references
  (let [d @index
        defs (query/definition-at d core 10 16)]
    (is (= "app.util/trimmed" (:fqn (first defs))))
    (is (>= (count (query/references d "app.util/trimmed")) 3))))

(deftest search-and-hover
  (let [d @index
        hits (query/search d "greet" 10)]
    (is (some #(= "app.core/greet" (:fqn %)) hits))
    (is (= "Say hi to name." (:doc (query/hover d "app.core/greet"))))))

(deftest diagnostics-find-real-problems
  (let [d @index
        ds (diagnostics/file-diagnostics d core)]
    (is (some #(= :unused-namespace (:code %)) ds))
    (is (not-any? #(= :unresolved-namespace (:code %)) ds))
    (is (some #(= :unused-private-var (:code %))
              (diagnostics/file-diagnostics d util)))))

(deftest rename-preview-updates-refer-and-use
  (let [d @index
        r (rename/rename d {:path core :row 10 :col 16 :to "trimmed*"})]
    (is (:ok r))
    (is (re-find #"trimmed\*" (get (:preview r) core)))
    (is (re-find #"trimmed\*" (get (:preview r) util)))))

(deftest clean-ns-is-idempotent-after-apply
  (let [tmp (str (fs/create-temp-dir))
        _ (fs/copy-tree "test/fixtures/simple" tmp)
        core2 (str tmp "/src/app/core.clj")
        d (db/build tmp)
        plan (clean-ns/plan d core2)]
    (is (:ok plan))
    (is (:changed plan))
    (spit core2 (:preview plan))
    (let [d2 (db/build tmp)
          plan2 (clean-ns/plan d2 core2)]
      (is (not (:changed plan2))))))

(deftest add-require-preview
  (let [d @index
        r (add-require/plan d {:path core :symbol "clojure.set/union"})]
    (is (:ok r))
    (is (re-find #"clojure\.set" (:preview r)))))

(deftest graph-finds-callers
  (let [d @index]
    (is (contains? (set (graph/impact d "app.core/secret")) "app.core/later"))
    (is (contains? (set (graph/impact d "app.util/trimmed")) "app.core/greet"))))

(deftest forms-outline
  (let [source (slurp core)]
    (is (= "greet" (-> (forms/top-level-forms source) (nth 2) :text
                       (clojure.string/split #"\s+") (nth 1))))))

(deftest tolerant-parse-keeps-prefix
  (let [r (read/parse "(ns a)\n(")]
    (is (false? (:ok r)))
    (is (= 1 (:count r)))))

(deftest text-roundtrip
  (doseq [s ["" "abc" "a\nb" "😀x" "a\r\nb"]]
    (doseq [i (range (inc (count s)))]
      (is (= i (text/position->offset s (text/offset->position s i)))))))
