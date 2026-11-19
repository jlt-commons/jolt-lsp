(ns jolt-lsp.kondo-parity-test
  "Checks that follow clj-kondo's linters: arity, unresolved vars, private
  and deprecated calls, forward references, local findings, hooks, and the
  incremental index keeping other files' resolution current."
  (:require [babashka.fs :as fs]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [jolt-lsp.db :as db]
            [jolt-lsp.diagnostics :as diagnostics]
            [jolt-lsp.query :as query]
            [jolt-lsp.settings]))

(defn- project
  "A throwaway project with these files ({relative-path text}). Returns the
  canonical root."
  [files]
  (let [root (str (fs/canonicalize (fs/create-temp-dir {:prefix "jolt-lsp-kp"})))]
    (spit (str root "/deps.edn") "{:paths [\"src\"]}")
    (doseq [[p text] files]
      (let [f (str root "/" p)]
        (fs/create-dirs (fs/parent f))
        (spit f text)))
    root))

(defn- diags
  "[code message] pairs for one file of a fresh index."
  ([root rel] (diags (db/build root) root rel))
  ([d root rel]
   (->> (diagnostics/file-diagnostics d (str root "/" rel))
        (mapv (juxt :code :message)))))

(defn- codes [ds] (set (map first ds)))

(defn- has? [ds code fragment]
  (boolean (some (fn [[c m]] (and (= c code) (string/includes? m fragment))) ds)))

;; ---------------------------------------------------------------------------
;; round 1: the incremental index

(deftest patching-a-file-re-resolves-its-dependents
  (let [root (project {"src/s/a.clj" "(ns s.a)\n(defn x [] 1)\n"
                       "src/s/b.clj" "(ns s.b (:require [s.a :refer :all]))\n(defn y [] (x) (z))\n"
                       "src/s/c.clj" "(ns s.c (:require [s.a :as a]))\n(defn w [] (a/z))\n"})
        d0 (db/build root)
        _ (is (has? (diags d0 root "src/s/b.clj") :unresolved-symbol "z"))
        d1 (db/update-file-text d0 root (str root "/src/s/a.clj")
                                "(ns s.a)\n(defn x [] 1)\n(defn z [] 2)\n")]
    (testing "a refer-all user sees the new var"
      (is (empty? (diags d1 root "src/s/b.clj"))))
    (testing "the definition and both users are references, as in a fresh build"
      (is (= 3 (count (query/references d1 "s.a/z")))))
    (testing "removing it again flags it again"
      (let [d2 (db/update-file-text d1 root (str root "/src/s/a.clj")
                                    "(ns s.a)\n(defn x [] 1)\n")]
        (is (has? (diags d2 root "src/s/b.clj") :unresolved-symbol "z"))))))

(deftest deprecated-metadata-is-indexed
  (let [root (project {"src/d/a.clj" (str "(ns d.a)\n"
                                          "(defn ^:deprecated old [] 1)\n"
                                          "(defn older {:deprecated \"1.2\"} [] 1)\n"
                                          "(def ^{:deprecated true} oldest 1)\n"
                                          "(defn fresh [] 1)\n")})
        d (db/build root)]
    (is (:deprecated (query/hover d "d.a/old")))
    (is (:deprecated (query/hover d "d.a/older")))
    (is (:deprecated (query/hover d "d.a/oldest")))
    (is (not (:deprecated (query/hover d "d.a/fresh"))))))

;; ---------------------------------------------------------------------------
;; round 2: arity of var calls

(def ^:private arity-a
  (str "(ns ar.a)\n"
       "(defn two [x y] (+ x y))\n"
       "(defn many [x & more] (apply + x more))\n"
       "(defn multi ([] 0) ([a] a))\n"
       "(def lit #(+ %1 %2))\n"
       "(def anon (fn [a] a))\n"
       "(defmacro mac [a b] `(list ~a ~b))\n"
       "(defprotocol P (pm [this x]))\n"))

(defn- arity-project [body]
  (project {"src/ar/a.clj" arity-a
            "src/ar/b.clj" (str "(ns ar.b (:require [ar.a :as a :refer [two]]))\n" body)}))

(defn- arity-diags [body]
  (let [root (arity-project body)]
    (filterv #(= :invalid-arity (first %)) (diags root "src/ar/b.clj"))))

(deftest arity-of-project-vars
  (is (has? (arity-diags "(two 1)") :invalid-arity "ar.a/two is called with 1 arg but expects 2"))
  (is (has? (arity-diags "(a/two 1 2 3)") :invalid-arity "with 3 args but expects 2"))
  (is (has? (arity-diags "(a/many)") :invalid-arity "expects 1 or more"))
  (is (has? (arity-diags "(a/multi 1 2)") :invalid-arity "expects 0 or 1"))
  (is (has? (arity-diags "(a/lit 1)") :invalid-arity "expects 2"))
  (is (has? (arity-diags "(a/anon)") :invalid-arity "expects 1"))
  (is (has? (arity-diags "(a/mac 1)") :invalid-arity "expects 2"))
  (is (has? (arity-diags "(a/pm 1 2 3)") :invalid-arity "expects 2"))
  (testing "matching calls are quiet"
    (is (empty? (arity-diags "(two 1 2) (a/many 1 2 3) (a/multi) (a/multi 1) (a/lit 1 2) (a/pm 1 2)")))))

(deftest arity-of-core-fns
  (is (has? (arity-diags "(assoc {} :a)") :invalid-arity "clojure.core/assoc is called with 2 args"))
  (is (has? (arity-diags "(clojure.string/join)") :invalid-arity "clojure.string/join"))
  (is (empty? (arity-diags "(assoc {} :a 1) (map inc [1]) (str)"))))

(deftest arity-through-threading
  (is (has? (arity-diags "(-> 1 (two 2 3))") :invalid-arity "with 3 args"))
  (is (has? (arity-diags "(->> 1 (two))") :invalid-arity "with 1 arg"))
  (is (has? (arity-diags "(-> {} (assoc :a))") :invalid-arity "with 2 args"))
  (is (has? (arity-diags "(-> 1 a/multi a/two)") :invalid-arity "ar.a/two is called with 1 arg"))
  (is (has? (arity-diags "(doto {} (assoc :a))") :invalid-arity "with 2 args"))
  (is (has? (arity-diags "(cond-> 1 true (two 1 2))") :invalid-arity "with 3 args"))
  (is (empty? (arity-diags "(-> 1 (two 2)) (->> 1 (two 2)) (-> 1 inc a/multi) (some-> 1 (two 2)) (doto {} (assoc :a 1))"))))

(deftest arity-is-not-checked-where-calls-are-not-code
  (is (empty? (arity-diags "(defmacro m [] `(two 1))")))
  (is (empty? (arity-diags "'(two 1)")))
  (is (empty? (arity-diags "#?(:cljs (two 1))"))))

;; ---------------------------------------------------------------------------
;; round 3: local fns and recur

(defn- local-diags [body]
  (let [root (project {"src/l/a.clj" (str "(ns l.a)\n" body)})]
    (filterv #(#{:invalid-arity :conflicting-fn-arity} (first %)) (diags root "src/l/a.clj"))))

(defn- local-diags*
  "Every diagnostic for a one-file project."
  [body]
  (let [root (project {"src/l/a.clj" (str "(ns l.a)\n" body)})]
    (diags root "src/l/a.clj")))

(deftest arity-of-local-fns
  (is (has? (local-diags "(let [g (fn [x] x)] (g 1 2))") :invalid-arity "g is called with 2 args but expects 1"))
  (is (has? (local-diags "(let [g #(inc %)] (g))") :invalid-arity "g is called with 0 args"))
  (is (has? (local-diags "(letfn [(h [x] x)] (h))") :invalid-arity "h is called with 0 args but expects 1"))
  (is (has? (local-diags "(fn self [x] (self))") :invalid-arity "self is called with 0 args"))
  (is (has? (local-diags "(let [g (fn [x] x)] (-> 1 (g 2)))") :invalid-arity "g is called with 2 args"))
  (testing "a rebinding or a param shadows the local fn"
    (is (empty? (local-diags "(let [g (fn [x] x) g inc] (g 1))")))
    (is (empty? (local-diags "(let [g (fn [x] x)] (fn [g] (g 1 2 3)))")))
    (is (empty? (local-diags "(let [g (fn [x] x)] (g 1))")))))

(deftest recur-arity
  (is (has? (local-diags "(loop [i 0] (recur 1 2))") :invalid-arity "recur argument count mismatch (expected 1, got 2)"))
  (is (has? (local-diags "(defn f [a b] (recur 1))") :invalid-arity "expected 2, got 1"))
  (is (has? (local-diags "(defn f ([a] (recur)) ([a b] a))") :invalid-arity "expected 1, got 0"))
  (is (empty? (local-diags "(defn f [a & r] (recur 1 nil)) (loop [a 1 b 2] (when a (recur b nil))) (defn g [x] (loop [] (recur)))")))
  (testing "recur inside a nested fn targets that fn"
    (is (empty? (local-diags "(defn f [a b] (map (fn [x] (recur x)) [a b]))")))))

(deftest conflicting-fn-arity
  (is (has? (local-diags "(defn f ([a] a) ([b] b))") :conflicting-fn-arity "More than one function overload with arity 1"))
  (is (has? (local-diags "(fn ([] 1) ([] 2))") :conflicting-fn-arity "arity 0")))

(deftest threading-forms-as-threading-steps
  (testing "a threading form that is a step has every item as a step"
    (is (empty? (arity-diags "(some-> 1 (->> (two 2)))")))
    (is (empty? (arity-diags "(-> {} (cond-> true (assoc :a 1)) (assoc :b 2))")))
    (is (has? (arity-diags "(-> 1 (->> (two 2 3)))") :invalid-arity "with 3 args"))))

(deftest recur-in-unknown-forms-is-not-checked
  (is (empty? (local-diags "(defmacro go-loop [b & body] `(loop ~b ~@body))\n(defn f [a b c] (go-loop [x 1] (recur (inc x))))")))
  (is (empty? (local-diags "(defn f [a b] (lazy-seq (recur 1)))")))
  (is (has? (local-diags "(defn f [a b] (if a (recur 1) (when b (recur 2 3))))") :invalid-arity "expected 2, got 1")))

(deftest skip-args-and-levels-config
  (let [root (project {"src/ar/a.clj" arity-a
                       "src/ar/b.clj" (str "(ns ar.b (:require [ar.a :as a :refer [two]] [ar.dsl :refer [law]]))\n"
                                           "(law (two 1))\n(two 1 2 3)\n(defn- unused [] 1)\n")
                       "src/ar/dsl.clj" "(ns ar.dsl)\n(defmacro law [& body] nil)\n"
                       ".clj-kondo/config.edn" (str "{:linters {:invalid-arity {:skip-args [ar.dsl/law]}"
                                                    " :unused-private-var {:level :off}}}")})
        ds (vec (diagnostics/file-diagnostics (db/build root) (str root "/src/ar/b.clj")
                                              {:levels (:levels (jolt-lsp.settings/lint-config root))}))]
    (is (= 1 (count (filter #(= :invalid-arity (:code %)) ds))))
    (is (= 3 (:row (first (filter #(= :invalid-arity (:code %)) ds)))))
    (is (not (some #(= :unused-private-var (:code %)) ds)))))

(deftest reader-conditional-threading-step
  (is (empty? (arity-diags "(->> 1 #?(:clj (two 2)))")))
  (is (has? (arity-diags "(->> 1 #?(:clj (two 2 3)))") :invalid-arity "with 3 args")))

;; ---------------------------------------------------------------------------
;; round 4: unresolved vars, private and deprecated calls, forward references

(def ^:private r4-a
  (str "(ns r4.a)\n"
       "(defn pub [] 1)\n"
       "(defn- secret [] 1)\n"
       "(def ^:private hidden 1)\n"
       "(defn ^:deprecated old [] 1)\n"
       "(defn uses-old [] (old))\n"
       "(defrecord Rec [a])\n"))

(defn- r4-diags [body & [extra]]
  (let [root (project (merge {"src/r4/a.clj" r4-a
                              "src/r4/b.clj" (str "(ns r4.b (:require [r4.a :as a] [clojure.string :as str]))\n" body)}
                             extra))]
    (diags root "src/r4/b.clj")))

(deftest unresolved-var
  (is (has? (r4-diags "(a/nope 1)") :unresolved-var "Unresolved var: a/nope"))
  (is (has? (r4-diags "(str/nope \"x\")") :unresolved-var "Unresolved var: str/nope"))
  (is (has? (r4-diags "r4.a/nope") :unresolved-var "r4.a/nope"))
  (testing "vars that do exist, or a namespace with defs the index cannot see"
    (is (empty? (filter #(= :unresolved-var (first %))
                        (r4-diags "(a/pub) (a/->Rec 1) (a/map->Rec {}) (str/join [])"))))
    (is (empty? (filter #(= :unresolved-var (first %))
                        (r4-diags "(m/made)" {"src/r4/m.clj" "(ns r4.m)\n(defstate made 1)\n"
                                              "src/r4/b.clj" "(ns r4.b (:require [r4.m :as m]))\n(m/made)\n"}))))
    (is (empty? (filter #(= :unresolved-var (first %)) (r4-diags "`(a/nope)"))))))

(deftest private-call
  (is (has? (r4-diags "(a/secret)") :private-call "#'r4.a/secret is private"))
  (is (has? (r4-diags "a/hidden") :private-call "#'r4.a/hidden is private"))
  (testing "var quote and syntax-quote may reach a private var"
    (is (empty? (filter #(= :private-call (first %)) (r4-diags "#'a/secret `(a/secret)"))))))

(deftest deprecated-var
  (is (has? (r4-diags "(a/old)") :deprecated-var "#'r4.a/old is deprecated"))
  (testing "not inside the deprecated var's own namespace definitions"
    (let [root (project {"src/r4/a.clj" r4-a})]
      (is (has? (diags root "src/r4/a.clj") :deprecated-var "#'r4.a/old is deprecated"))
      (is (= 1 (count (filter #(= :deprecated-var (first %)) (diags root "src/r4/a.clj"))))))))

(deftest forward-reference
  (let [ds (local-diags* "(defn early [] (later 1))\n(defn later [x] x)\n(defn ok [] (later 2))\n")]
    (is (has? ds :unresolved-symbol "Unresolved symbol: later"))
    (is (= 1 (count (filter #(= :unresolved-symbol (first %)) ds)))))
  (testing "declare, recursion and rich comments are fine"
    (is (empty? (local-diags* "(declare later)\n(defn early [] (later 1))\n(defn later [x] x)\n")))
    (is (empty? (local-diags* "(defn f [x] (when x (f nil)))\n")))
    (is (empty? (local-diags* "(comment (later 1))\n(defn later [x] x)\n")))))

;; ---------------------------------------------------------------------------
;; unresolved-symbol false positives found linting sibling projects

(defn- unresolved [body]
  (->> (local-diags* body)
       (filter #(= :unresolved-symbol (first %)))
       (map second)
       vec))

(deftest qualified-core-def-forms
  (is (empty? (unresolved "(clojure.core/defn f [form] form)\n(f 1)")))
  (is (empty? (unresolved "(clojure.core/let [a 1] a)")))
  (is (empty? (unresolved "(ns l.a (:require [clojure.core :as c]))\n(c/defn g [x] x) (c/fn [y] y) (g 1)"))))

(deftest declare-with-metadata
  (is (empty? (unresolved "(declare ^:dynamic *p*)\n(defn f [] *p*)\n(def ^:dynamic *p* 1)"))))

(deftest areduce-and-amap-bind
  (is (empty? (unresolved "(defn f [x] (areduce x i s 0.0 (+ s (aget x i))))")))
  (is (empty? (unresolved "(defn f [x] (amap x i r (inc (aget x i))))"))))

(deftest reader-conditional-params-and-meta-protocol-names
  (is (empty? (unresolved "(defn g (#?(:clj ^long [p] :cljs ^number [p]) (inc p)))")))
  (is (empty? (unresolved "(defn g #?(:clj [p] :cljs [q]) p)")))
  (is (empty? (unresolved "(defprotocol P (^Object get-v [self]))\n(defn h [x] (get-v x))"))))

(deftest refer-all-from-an-unknown-namespace
  (is (empty? (unresolved "(ns l.a (:require [some.unindexed.lib :refer :all]))\n(defne stepo [a] (foo a))")))
  (testing "a qualified unknown alias is still flagged as a namespace problem, not hidden"
    (is (empty? (unresolved "(ns l.a (:require [some.unindexed.lib :refer :all]))\n(x/y 1)")))))

(deftest writ-spec-forms-name-vars-as-data
  (let [root (project {"src/w/a.clj" "(ns w.a)\n(defn- hidden [x] x)\n(defn ns-name [a b] a)\n"
                       "src/w/spec.clj" "(ns w.spec (:require [w.a :as a] [writ.spec :refer [law calls]]))\n(calls a/hidden {:through []})\n(law (ns-name 1 2))\n"})]
    (is (empty? (filter #(#{:private-call :invalid-arity} (first %)) (diags root "src/w/spec.clj"))))))

(deftest unknown-def-like-macros-define-their-name
  (is (empty? (unresolved "(ns l.a (:require [x.lc :as lc]))\n(lc/defword perform [id] id)\n(def spawn perform)"))))

(deftest splicing-reader-conditional-in-a-libspec
  (let [root (project {"src/g/gll.clj" "(ns g.gll)\n(defmacro profile [& body] `(do ~@body))\n"
                       "src/g/r.clj" "(ns g.r (:require [g.gll :as gll #?@(:clj [:refer [profile]])]))\n(profile 1)\n"})]
    (is (empty? (filter #(= :unresolved-symbol (first %)) (diags root "src/g/r.clj"))))))
