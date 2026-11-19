(ns jolt-lsp.walk-gaps-test
  "Analyser false positives: #_ in binding vectors, as-> inside threading
  macros, proxy-super's method name, and defs inside (comment ...)."
  (:require [clojure.test :refer [deftest is testing]]
            [jolt-lsp.analyze :as analyze]
            [jolt-lsp.tree :as tree]))

(defn- locals-and-occs [src]
  (let [r (analyze/analyze src "x.clj")]
    {:local-names (set (map :name (:locals r)))
     :refs (set (map :name (:local-refs r)))
     :occs (set (map :text (:occurrences r)))}))

(defn- unresolved-ish
  "Occurrence texts that neither resolve to a local nor are core: with a
  made-up name nothing defines, what is left is what the analyser could not
  bind."
  [src nm]
  (let [r (analyze/analyze src "x.clj")]
    {:occ (some #(= nm (:text %)) (:occurrences r))
     :local-ref (some #(= nm (:name %)) (:local-refs r))}))

(defn- free? [src nm]
  (let [{:keys [occ local-ref]} (unresolved-ish src nm)]
    (and occ (not local-ref))))

(deftest discarded-forms-do-not-shift-let-pairs
  (is (not (free? "(ns x)\n(defn f []\n  (let [a #_{:x 1} (inc 1)\n        b (atom a)]\n    (deref b)))" "b")))
  (is (not (free? "(ns x)\n(defn f [] (loop [#_ignored a 1 b (inc a)] (recur b b)))" "a")))
  (is (not (free? "(ns x)\n(defn f [m] (with-open [r #_{:k 1} (open m) w (wrap r)] (use w)))" "r")))
  (is (not (free? "(ns x)\n(defn f [m] (when-let [a #_x (get m 1)] (inc a)))" "a")))
  (is (not (free? "(ns x)\n(defn f [m] (for [a m #_:when #_(odd? a) :let [b #_z (inc a)] c [b]] [a c]))" "b")))
  (is (not (free? "(ns x)\n(defn f [m] (doseq [a #_q m b [a]] (prn b)))" "b"))))

(deftest as-arrow-threaded-binds-its-name
  (is (not (free? "(ns x)\n(defn f [v] (some-> v (as-> s (str s \"!\"))))" "s")))
  (is (not (free? "(ns x)\n(defn f [v] (-> v (as-> s (str s \"!\")) str))" "s")))
  (is (not (free? "(ns x)\n(defn f [v] (->> v (as-> s (str s \"!\"))))" "s")))
  (is (not (free? "(ns x)\n(defn f [v] (cond-> v true (as-> s (str s \"!\"))))" "s")))
  (testing "a plain as-> still takes expr first"
    (is (not (free? "(ns x)\n(defn f [v] (as-> v s (str s \"!\")))" "s")))))

(deftest proxy-super-method-name-is-not-a-reference
  (let [src "(ns x)\n(defn f [] (proxy [java.io.Reader] [] (close [] (proxy-super close)) (skip [n] (proxy-super skip n))))"]
    (is (not (free? src "close")))
    (is (not (free? src "skip")))
    (is (not (free? src "n"))))
  (is (free? "(ns x)\n(defn f [] (proxy [java.io.Reader] [] (skip [n] (proxy-super skip mystery))))" "mystery")))

(deftest comment-defs-resolve-inside-the-comment
  (let [src "(ns x)\n(comment\n  (def srv (start))\n  (defn helper [] 1)\n  (stop srv)\n  (helper))"]
    (is (not (free? src "srv")))
    (is (not (free? src "helper")))
    (testing "they are still not project definitions"
      (is (empty? (:vars (analyze/analyze src "x.clj"))))))
  (testing "and do not leak out of the comment"
    (is (free? "(ns x)\n(comment (def srv 1))\n(inc srv)" "srv"))))
