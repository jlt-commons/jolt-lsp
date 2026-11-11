(ns jolt-lsp.analyze-def-test
  (:require [clojure.test :refer [deftest is]]
            [jolt-lsp.analyze :as analyze]))

(defn- occurrence-texts [src]
  (set (map :text (:occurrences (analyze/analyze src "x.clj")))))

(defn- var-named [src nm]
  (first (filter #(= nm (:name %)) (:vars (analyze/analyze src "x.clj")))))

(def ^:private header "(ns x (:require [y :as y])) ")

(deftest a-def-whose-value-is-a-map-records-its-refs
  (is (contains? (occurrence-texts (str header "(def m {:k y/f})")) "y/f"))
  (is (contains? (occurrence-texts (str header "(def m \"doc\" {:k y/f})")) "y/f"))
  (is (contains? (occurrence-texts (str header "(defonce m {:k (fn [r] (y/f r))})")) "y/f")))

(deftest a-defmulti-attr-map-is-skipped-but-its-dispatch-is-walked
  (is (contains? (occurrence-texts (str header "(defmulti m {:arglists '([x])} y/f)")) "y/f"))
  (is (contains? (occurrence-texts (str header "(defmulti m {:k y/f})")) "y/f")))

(deftest a-map-value-is-not-an-attr-map
  (is (false? (:private (var-named (str header "(def m {:private true})") "m"))))
  (is (true? (:private (var-named (str header "(defn f {:private true} [] 1)") "f")))))
