(ns jolt-lsp.refactor-test
  (:require [clojure.test :refer [deftest is]]
            [babashka.fs :as fs]
            [jolt-lsp.db :as db]
            [jolt-lsp.refactor :as refactor]))

(def root (str (fs/canonicalize "test/fixtures/refactor")))
(def core (str root "/src/demo/core.clj"))

(defonce index (delay (db/build root)))

(defn- local-row [name]
  (some #(when (= name (:name %)) %) (db/local-defs-in-file @index (:id (db/file-by-path @index core)))))

(deftest sort-requires-sorts
  (let [r (refactor/sort-requires @index core)
        text (get (:preview r) core)]
    (is (:ok r))
    (is (< (clojure.string/index-of text "clojure.set")
           (clojure.string/index-of text "clojure.string")))))

(deftest inline-single-use
  (let [l (local-row "y")
        r (refactor/inline-symbol @index core (:row l) (:col l))]
    (is (:ok r))
    (is (re-find #"\(\* 2 \(inc x\)\)" (get (:preview r) core)))))

(deftest destructure-map-uses
  (let [l (local-row "v")
        r (refactor/destructure-keys @index core (:row l) (:col l))]
    (is (:ok r))
    (is (re-find #"\{:keys \[a b\]\}" (get (:preview r) core)))
    (is (re-find #"\(\+ a b\)" (get (:preview r) core)))))

(deftest cycle-auto-keyword
  (let [file (db/file-by-path @index core)
        k (first (db/keyword-refs-in-file @index (:id file)))]
    (is (= ":demo.core/thing" (:fqn k)))
    (let [r (refactor/cycle-keyword @index core (:row k) (:col k))]
      (is (:ok r))
      (is (re-find #":demo\.core/thing" (get (:preview r) core))))))

(deftest restructure-bindings
  (let [file (db/file-by-path @index core)
        l (some #(when (= "a" (:name %)) %) (db/local-defs-in-file @index (:id file)))
        r (refactor/restructure-keys @index core (:row l) (:col l))]
    (is (:ok r))
    (is (re-find #"\{:keys \[a b\]\} m" (get (:preview r) core)))
    (is (not (re-find #"\(:a m\)" (get (:preview r) core))))))
