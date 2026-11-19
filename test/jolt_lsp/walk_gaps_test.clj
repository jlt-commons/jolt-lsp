(ns jolt-lsp.walk-gaps-test
  "Analyser false positives: #_ in binding vectors, as-> inside threading
  macros, proxy-super's method name, and defs inside (comment ...)."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [jolt-lsp.analyze :as analyze]
            [jolt-lsp.db :as db]
            [jolt-lsp.edit.rename :as rename]
            [jolt-lsp.query :as query]))

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

(defn- bound-local?
  "nm is declared as a local and referenced as one, and no reference to it
  is left free."
  [src nm]
  (let [r (analyze/analyze src "x.clj")]
    (and (some #(= nm (:name %)) (:locals r))
         (some #(= nm (:name %)) (:local-refs r))
         (not (free? src nm)))))

(deftest discarded-forms-do-not-shift-let-pairs
  (is (bound-local? "(ns x)\n(defn f []\n  (let [a #_{:x 1} (inc 1)\n        b (atom a)]\n    (deref b)))" "b"))
  (is (bound-local? "(ns x)\n(defn f [] (loop [#_ignored a 1 b (inc a)] (recur b b)))" "a"))
  (is (bound-local? "(ns x)\n(defn f [m] (with-open [r #_{:k 1} (open m) w (wrap r)] (use w)))" "r"))
  (is (bound-local? "(ns x)\n(defn f [m] (when-let [a #_x (get m 1)] (inc a)))" "a"))
  (is (bound-local? "(ns x)\n(defn f [m] (for [a m #_:when #_(odd? a) :let [b #_z (inc a)] c [b]] [a c]))" "b"))
  (is (bound-local? "(ns x)\n(defn f [m] (doseq [a #_q m b [a]] (prn b)))" "b")))

(deftest stacked-and-map-discards
  (is (bound-local? "(ns x)\n(defn f [] (let [#_ #_ q 1 a 2 b (inc a)] b))" "b"))
  (is (bound-local? "(ns x)\n(defn f [] (let [#_ #_ q 1 a 2 b (inc a)] b))" "a"))
  (is (bound-local? "(ns x)\n(defn f [m] (let [{#_a b :k} m] b))" "b"))
  (is (bound-local? "(ns x)\n(defn f [m] (let [{b :k :or {#_z b 1}} m] b))" "b"))
  (testing "case clauses: a discard before a result must not turn it into a test constant"
    (is (bound-local? "(ns x)\n(defn f [k] (let [w 1] (case k :a #_:skip (inc w) :b 2)))" "w"))))

(deftest as-arrow-spellings-and-cond-test
  (is (bound-local? "(ns x)\n(defn f [v] (-> v (clojure.core/as-> s (str s))))" "s"))
  (is (bound-local? "(ns x (:require [clojure.core :as c]))\n(defn f [v] (-> v (c/as-> s (str s))))" "s"))
  (is (bound-local? "(ns x)\n(defn f [v] (cond-> v (pos? v) (as-> s (+ s 1))))" "s"))
  (testing "a test position is walked as plain code"
    (is (free? "(ns x)\n(defn f [v] (cond-> v (pos? mystery) inc))" "mystery"))))

(deftest as-arrow-threaded-binds-its-name
  (is (bound-local? "(ns x)\n(defn f [v] (some-> v (as-> s (str s \"!\"))))" "s"))
  (is (bound-local? "(ns x)\n(defn f [v] (-> v (as-> s (str s \"!\")) str))" "s"))
  (is (bound-local? "(ns x)\n(defn f [v] (->> v (as-> s (str s \"!\"))))" "s"))
  (is (bound-local? "(ns x)\n(defn f [v] (cond-> v true (as-> s (str s \"!\"))))" "s"))
  (testing "a plain as-> still takes expr first"
    (is (bound-local? "(ns x)\n(defn f [v] (as-> v s (str s \"!\")))" "s"))))

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

(deftest proxy-this-has-no-declaration
  (let [src "(ns x)\n(defn f [] (proxy [Object] [] (toString [] (str this))))\n"
        root (str (fs/canonicalize (fs/create-temp-dir {:prefix "jolt-lsp-this"})))
        _ (spit (str root "/deps.edn") "{:paths [\"src\"]}")
        _ (fs/create-dirs (str root "/src"))
        path (str root "/src/x.clj")
        _ (spit path src)
        d (db/build root)
        this-col (inc (.indexOf ^String (nth (clojure.string/split-lines src) 1) "(str this") )]
    (testing "no local declaration named this"
      (is (not-any? #(= "this" (:name %)) (db/local-defs-in-file d (:id (db/file-by-path d path))))))
    (testing "rename at this makes no edit on the method name"
      (let [r (try (rename/rename d {:path path :row 2 :col (+ this-col 5) :to "me"})
                   (catch Throwable e {:error (ex-message e)}))
            edits (pr-str r)]
        (is (not (re-find #"toString" edits)) edits)))
    (testing "definition of this does not point at the method name"
      (is (empty? (query/definition-at d path 2 (+ this-col 5)))))))
