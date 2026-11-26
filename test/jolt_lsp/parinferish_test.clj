(ns jolt-lsp.parinferish-test
  "parinferish's string tokens: where a string literal ends, escapes included."
  (:require [clojure.test :refer [deftest is testing]]
            [parinferish.core :as p]))

(defn- strings [s]
  (->> (tree-seq sequential? seq (p/parse s))
       (filter #(and (vector? %) (= :string (first %))))
       (mapv second)))

(deftest a-string-ends-at-its-closing-quote
  (is (= ["\"abc\""] (strings "(f \"abc\" x)")))
  (is (= ["\"\""] (strings "\"\""))))

(deftest an-escaped-quote-does-not-end-it
  (is (= ["\"a\\\"b\""] (strings "(f \"a\\\"b\" x)")))
  (is (= ["\"a\\\\\""] (strings "\"a\\\\\" x"))))

(deftest an-unterminated-string-runs-to-the-end
  (is (= ["\"abc x"] (strings "\"abc x")))
  (testing "a trailing backslash is kept"
    (is (= ["\"ab\\"] (strings "\"ab\\")))))

(deftest round-trips
  (doseq [s ["(f \"a\\\"b\" x)" "\"abc" "\"ab\\" "(str \"x\\ny\" \"\")"]]
    (is (= s (p/flatten (p/parse s))) s)))

(deftest a-long-string-is-read-in-one-pass
  (let [body (apply str (repeat 200000 "ab\\\"c"))
        s (str "(def x \"" body "\")")]
    (is (= [(str "\"" body "\"")] (strings s)))))

(defn- tokens [s]
  (->> (tree-seq sequential? seq (p/parse s))
       (filter #(and (vector? %) (keyword? (first %)) (string? (second %))))
       (mapv (juxt first second))))

(deftest tokens-longer-than-the-match-window-are-whole
  (let [sym (apply str "x" (repeat 999 "y"))
        cmt (apply str ";" (repeat 999 "c"))]
    (is (= [[:delimiter "("] [:symbol "f"] [:whitespace " "] [:symbol sym] [:delimiter ")"]]
           (tokens (str "(f " sym ")"))))
    (is (some #{[:comment cmt]} (tokens (str "(f)\n" cmt "\n(g)"))))
    (testing "and so are ones that end exactly where a window does"
      (doseq [k (range 250 262)]
        (let [s (apply str (repeat k "z"))]
          (is (= [[:symbol s] [:whitespace " "] [:symbol "q"]] (tokens (str s " q"))) k))))))

(deftest a-large-input-round-trips
  (let [s (apply str (for [i (range 5000)] (str "(defn f" i " [x] ;; note\n  (str \"a\\\"" i "\" x))\n")))]
    (is (= s (p/flatten (p/parse s))))
    (is (= s (p/flatten (p/parse s {:mode :indent}))))))
