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
            [jolt-lsp.settings]
            [jolt-lsp.api]
            [jolt-lsp.cli]))

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

(defn- arity-diags* [body]
  (diags (arity-project body) "src/ar/b.clj"))

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

;; ---------------------------------------------------------------------------
;; round 5: clj-kondo hooks

(def ^:private hook-src
  (str "(ns hooks.lib (:require [clj-kondo.hooks-api :as api] [hooks.util :as u]))\n"
       "(defn defthing [{:keys [node]}]\n"
       "  (let [[_ nm & body] (:children node)]\n"
       "    (when-not nm (api/reg-finding! (assoc (meta node) :message \"defthing needs a name\" :type :defthing)))\n"
       "    {:node (with-meta (api/list-node (list* (api/token-node 'def) nm body)) (meta node))}))\n"
       "(defn with-x [{:keys [node]}]\n"
       "  (let [[_ bindings & body] (:children node)]\n"
       "    {:node (u/let-node bindings body)}))\n"
       "(defn boom [_] (throw (ex-info \"kaboom\" {})))\n"
       "(defmacro unless [c & body] `(if ~c nil (do ~@body)))\n"))

(def ^:private hook-util
  (str "(ns hooks.util (:require [clj-kondo.hooks-api :as api]))\n"
       "(defn let-node [bindings body]\n"
       "  (api/list-node (list* (api/token-node 'let) bindings body)))\n"))

(def ^:private lib-src
  (str "(ns my.lib)\n"
       "(defmacro defthing [nm & body] `(def ~nm ~@body))\n"
       "(defmacro with-x [b & body] `(let ~b ~@body))\n"
       "(defmacro boom [& body] nil)\n"
       "(defmacro unless [c & body] `(if ~c nil (do ~@body)))\n"))

(def ^:private hook-config
  (str "{:hooks {:analyze-call {my.lib/defthing hooks.lib/defthing"
       "                         my.lib/with-x hooks.lib/with-x"
       "                         my.lib/boom hooks.lib/boom}"
       "         :macroexpand {my.lib/unless hooks.lib/unless}}}"))

(defn- hook-project [body & [extra]]
  (project (merge {"src/my/lib.clj" lib-src
                   "src/app/core.clj" (str "(ns app.core (:require [my.lib :refer [defthing with-x boom unless]]))\n" body)
                   ".clj-kondo/config.edn" hook-config
                   ".clj-kondo/hooks/lib.clj" hook-src
                   ".clj-kondo/hooks/util.clj" hook-util}
                  extra)))

(deftest analyze-call-hook-defines-a-var
  (let [root (hook-project "(defthing foo 1)\n(defn use-it [] (inc foo))\n")
        d (db/build root)]
    (is (empty? (remove #(= :unused-referred-var (first %)) (diags d root "src/app/core.clj"))))
    (is (some? (db/var-by-fqn d "app.core/foo")))
    (is (= 2 (count (query/references d "app.core/foo"))))))

(deftest analyze-call-hook-binds-locals-through-a-helper-ns
  (let [root (hook-project "(defn f [] (with-x [a 1] (inc a)))\n(defn g [] (with-x [b 1] (inc nope)))\n")
        ds (diags root "src/app/core.clj")]
    (is (has? ds :unresolved-symbol "Unresolved symbol: nope"))
    (is (has? ds :unused-binding "Unused binding: b"))
    (is (not (has? ds :unresolved-symbol "Unresolved symbol: a")))))

(deftest hook-findings-and-failures
  (let [ds (diags (hook-project "(defthing)\n(boom 1)\n") "src/app/core.clj")]
    (is (has? ds :defthing "defthing needs a name"))
    (is (has? ds :hook "kaboom"))))

(deftest macroexpand-hook-keeps-locations
  (let [root (hook-project "(defn f [x] (unless x (undefined-thing)))\n")
        d (db/build root)
        ds (remove #(= :unused-referred-var (:code %))
                   (diagnostics/file-diagnostics d (str root "/src/app/core.clj")))]
    (is (= [[:unresolved-symbol 2]] (mapv (juxt :code :row) ds)))
    (is (= 24 (:col (first ds))))))

(deftest hooks-from-imported-configs
  (let [root (project {"src/my/lib.clj" lib-src
                       "src/app/core.clj" "(ns app.core (:require [my.lib :refer [defthing]]))\n(defthing foo 1)\n(inc foo)\n"
                       ".clj-kondo/imports/my/lib/config.edn" "{:hooks {:analyze-call {my.lib/defthing hooks.lib/defthing}}}"
                       ".clj-kondo/imports/my/lib/hooks/lib.clj" hook-src
                       ".clj-kondo/imports/my/lib/hooks/util.clj" hook-util})]
    (is (empty? (diags root "src/app/core.clj")))))

(deftest hooks-can-be-turned-off
  (let [root (hook-project "(defthing)\n" {".jolt-lsp/config.edn" "{:hooks false}"})]
    (is (not (has? (diags root "src/app/core.clj") :defthing "needs a name")))))

(deftest hook-output-text-reads-back
  (let [root (hook-project "(with-x [a 1 b 2] (+ a b))\n")
        d (db/build root)]
    (is (empty? (remove #(= :unused-referred-var (first %)) (diags d root "src/app/core.clj"))))))

;; ---------------------------------------------------------------------------
;; round 6: clj-kondo's local linters

(defn- only [code body]
  (filterv #(= code (first %)) (local-diags* body)))

(deftest redundant-do
  (is (seq (only :redundant-do "(defn f [c] (do (prn c) c))")))
  (is (seq (only :redundant-do "(defn f [c] (let [a 1] (do (prn a) c)))")))
  (is (seq (only :redundant-do "(defn f [c] (do c))")))
  (is (seq (only :redundant-do "(defn f [c] (when c (do (prn c) c)))")))
  (testing "parse/read-coll*'s old shape"
    (is (seq (only :redundant-do "(defn- read-coll* [c n]\n  (do\n    (dotimes [_ n] (prn c))\n    (loop [acc []] acc)))"))))
  (testing "a do that groups forms where one form is expected is fine"
    (is (empty? (only :redundant-do "(defn f [c] (if c (do (prn c) 1) 2))")))
    (is (empty? (only :redundant-do "(defn f [c] [(do (prn c) 1)])")))))

(deftest redundant-let
  (is (seq (only :redundant-let "(defn f [] (let [a 1] (let [b a] b)))")))
  (is (empty? (only :redundant-let "(defn f [] (let [a 1] (prn a) (let [b a] b)))"))))

(deftest loop-without-recur
  (is (= [[:loop-without-recur "Loop without recur."]] (only :loop-without-recur "(defn f [] (loop [a 1] a))")))
  (is (empty? (only :loop-without-recur "(defn f [] (loop [a 1] (when (pos? a) (recur (dec a)))))")))
  (testing "a recur in a nested fn is that fn's"
    (is (seq (only :loop-without-recur "(defn f [] (loop [a 1] (fn [x] (recur x))))")))))

(deftest missing-body-in-when
  (is (seq (only :missing-body-in-when "(defn f [x] (when x))")))
  (is (empty? (only :missing-body-in-when "(defn f [x] (when x 1))"))))

(deftest format-arg-count
  (is (has? (local-diags* "(defn f [] (format \"%s %s\" 1))") :format "Format string expects 2 arguments instead of 1."))
  (is (has? (local-diags* "(defn f [] (printf \"%d\\n\" 1 2))") :format "expects 1 arguments instead of 2"))
  (is (empty? (only :format "(defn f [] (format \"%s %2$s %%\" 1 2) (format \"%s%n\" 1) (format fmt 1))"))))

(deftest unused-value
  (is (has? (local-diags* "(defn f [m] (assoc m :a 1) m)") :unused-value "Unused value"))
  (is (has? (local-diags* "(defn f [m] 1 m)") :unused-value "Unused value: 1"))
  (is (has? (local-diags* "(defn f [m] m m)") :unused-value "Unused value: m"))
  (is (has? (local-diags* "(defn f [m] (let [a 1] :k a))") :unused-value "Unused value: :k"))
  (testing "the last form, side effects, and a fn's condition map are fine"
    (is (empty? (only :unused-value "(defn f [m] (prn m) (swap! m inc) (assoc m :a 1))")))
    (is (empty? (only :unused-value "(defn f [m] {:pre [(map? m)]} m)")))))

(deftest redefined-var
  (is (has? (local-diags* "(defn f [] 1)\n(defn f [] 2)") :redefined-var "redefined var #'l.a/f"))
  (is (empty? (only :redefined-var "(declare f)\n(defn f [] 1)"))))

(deftest inline-def
  (is (has? (local-diags* "(defn f [] (def x 1))") :inline-def "inline def"))
  (is (empty? (only :inline-def "(let [a 1] (defn f [] a))"))))

(deftest type-mismatch
  (is (has? (local-diags* "(defn f [] (inc \"s\"))") :type-mismatch "Expected: number, received: string."))
  (is (has? (local-diags* "(defn f [] (+ 1 :a))") :type-mismatch "received: keyword"))
  (is (has? (local-diags* "(defn f [] (inc (str 1)))") :type-mismatch "received: string"))
  (is (has? (local-diags* "(defn f [] (clojure.string/join \",\" :k))") :type-mismatch "Expected: seqable collection"))
  (testing "unknown types never mismatch"
    (is (empty? (only :type-mismatch "(defn f [x] (inc x) (+ 1 (count x)) (-> \"s\" inc) (map inc [1 2]))")))
    (is (empty? (only :type-mismatch "(defn inc [x] x)\n(defn g [] (inc \"s\"))")))))

(deftest if-let-branches-are-not-a-body
  (is (empty? (filter #(#{:unused-value :redundant-do} (first %))
                      (local-diags* "(defn f [m] (if-let [v (get m 1)] {:v v} (do (prn m) 2)) (if-some [v m] v (assoc m :a 1)))")))))

(deftest locals-shadow-special-forms
  (let [ds (local-diags* "(defn f [m] (let [{:keys [fn arg]} m] (if arg (fn (arg 1)) (fn))))")]
    (is (empty? (filter #(#{:unused-value :unresolved-symbol :unused-binding} (first %)) ds)))))

(deftest for-bodies-and-fn-literal-dos
  (is (empty? (only :redundant-do "(defn f [xs] (for [x xs] (do (prn x) x)) #(do 1) #(do %))"))))

(deftest recur-inside-macros-and-or-counts-for-the-loop
  (is (empty? (only :loop-without-recur "(defn f [] (loop [] (receive [m (do (prn m) (recur))])))")))
  (is (empty? (local-diags* "(defn f [xs] (loop [a xs] (or (first a) (recur (rest a)))))"))))

(deftest refer-clojure-exclude-is-respected
  (is (has? (local-diags* "(ns l.a (:refer-clojure :exclude [inc]))\n(defn f [] (inc 1))") :unresolved-symbol "inc"))
  (is (empty? (only :type-mismatch "(ns l.a (:refer-clojure :exclude [==]) (:require [some.logic :refer :all]))\n(defn f [q] (== q [1 2]))"))))

(deftest reader-conditional-defs-are-not-redefinitions
  (is (empty? (only :redefined-var "#?(:clj (defn f [] 1) :cljs (defn f [] 2))"))))

(deftest threaded-when-has-its-test
  (is (empty? (only :missing-body-in-when "(defn f [x] (-> x nil? (when (throw (ex-info \"x\" {})))))"))))

(deftest syntax-quoted-special-forms-walk-their-unquotes
  (is (empty? (only :unused-binding "(defmacro defword [sym args & body] `(def ~sym (fn ~sym ~args ~@body)))"))))

(deftest unrequired-but-known-namespaces
  (let [root (project {"src/n/a.clj" "(ns n.a)\n(defn f [] 1)\n"
                       "src/n/b.clj" "(ns n.b)\n(defn g [] (n.a/f) (nope.ns/x) (jolt.host/foo))\n"})
        ds (diagnostics/file-diagnostics (db/build root) (str root "/src/n/b.clj"))
        by (into {} (map (juxt :message :severity)) (filter #(= :unresolved-namespace (:code %)) ds))]
    (is (= :warning (get by "Unresolved namespace n.a. Are you missing a require?")))
    (is (= :error (get by "Unknown namespace: nope.ns")))
    (is (= 2 (count by)))))

(deftest top-level-require-adds-aliases
  (let [root (project {"src/t/gui.clj" "(ns t.gui)\n(def tab (atom 0))\n"
                       "src/t/h.clj" "(ns t.h)\n(require '[t.gui :as gui])\n(defn f [] @gui/tab)\n"})]
    (is (empty? (diags root "src/t/h.clj")))))

(deftest referred-var-used-via-alias
  (is (has? (arity-diags* "(a/two 1 2)") :aliased-referred-var "Var two is referred but used via alias: a")))

;; ---------------------------------------------------------------------------
;; type inference: locals, return types, hints, user specs

(defn- types-in [files rel]
  (let [root (project files)]
    (filterv #(= :type-mismatch (first %)) (diags root rel))))

(defn- types-of [body]
  (types-in {"src/ti/a.clj" (str "(ns ti.a)\n" body)} "src/ti/a.clj"))

(deftest let-locals-carry-their-init-type
  (is (has? (types-of "(defn f [] (let [s \"x\"] (inc s)))") :type-mismatch "received: string"))
  (is (has? (types-of "(defn f [] (let [s (str 1)] (inc s)))") :type-mismatch "received: string"))
  (is (empty? (types-of "(defn f [n] (let [s n] (inc s)))")))
  (testing "a shadowing binding drops the type"
    (is (empty? (types-of "(defn f [n] (let [s \"x\" s n] (inc s)))")))))

(deftest project-fn-return-types
  (is (has? (types-of "(defn f [] \"x\")\n(defn g [] (inc (f)))") :type-mismatch "received: string"))
  (is (has? (types-of "(defn f [] \"x\")\n(defn g [] (f))\n(defn h [] (inc (g)))") :type-mismatch "received: string"))
  (is (has? (types-of "(defn f [x] (let [s (str x)] s))\n(defn g [] (inc (f 1)))") :type-mismatch "received: string"))
  (is (has? (types-in {"src/ti/a.clj" "(ns ti.a (:require [clojure.string :as s]))\n(defn f [] (s/join [1]))\n"
                       "src/ti/b.clj" "(ns ti.b (:require [ti.a :as a]))\n(defn g [] (inc (a/f)))\n"}
                      "src/ti/b.clj")
            :type-mismatch "received: string"))
  (testing "if is the union of its branches"
    (is (has? (types-of "(defn h [x] (if x \"a\" :b))\n(defn g [] (inc (h 1)))") :type-mismatch "received: keyword or string"))
    (is (empty? (types-of "(defn h [x] (if x \"a\" 1))\n(defn g [] (inc (h 1)))"))))
  (testing "mutual recursion terminates"
    (is (empty? (types-of "(declare b)\n(defn a [] (b))\n(defn b [] (a))\n(defn c [] (inc (a)))")))))

(deftest type-hints
  (is (has? (types-of "(defn k [^String s] s)\n(defn g [] (k 1))") :type-mismatch "Expected: string or nil, received: positive integer."))
  (is (has? (types-of "(defn m [^long n] (subs n 1))") :type-mismatch "received: long"))
  (is (has? (types-of "(defn ^String r [x] (identity x))\n(defn g [] (inc (r 1)))") :type-mismatch "received: string or nil"))
  (is (has? (types-of "(defn r ^String [x] (identity x))\n(defn g [] (inc (r 1)))") :type-mismatch "received: string or nil")))

(deftest def-values-have-types
  (is (has? (types-of "(def pi \"3\")\n(defn g [] (inc pi))") :type-mismatch "received: string")))

(deftest user-type-specs
  (is (has? (types-in {"src/ti/a.clj" "(ns ti.a)\n(defn f [x] x)\n(defn g [] (f :k))\n"
                       ".clj-kondo/config.edn" "{:linters {:type-mismatch {:namespaces {ti.a {f {:arities {1 {:args [:string] :ret :string}}}}}}}}"}
                      "src/ti/a.clj")
            :type-mismatch "Expected: string, received: keyword.")))

(deftest inference-false-positives
  (testing "dynamic and nil-valued defs are placeholders"
    (is (empty? (types-of "(def ^:dynamic *why* nil)\n(defn f [] (swap! *why* conj 1))")))
    (is (empty? (types-of "(def state nil)\n(defn f [] (swap! state inc))"))))
  (testing "if-some and when-let locals are not nil inside"
    (is (empty? (types-of "(def ^:dynamic *s* nil)\n(defn f [] (if-some [c *s*] (swap! c inc) 1))")))
    (is (empty? (types-of "(defn g [x] (when x (atom 1)))\n(defn f [] (when-let [c (g 1)] (swap! c inc)))"))))
  (testing "loop locals are rebound by recur"
    (is (empty? (types-of "(defn f [xs] (loop [m nil xs xs] (if (seq xs) (recur (inc (or m 0)) (rest xs)) (inc m))))")))))

;; ---------------------------------------------------------------------------
;; lint: the CI gate

(deftest lint-gate
  (let [root (project {"src/g/a.clj" "(ns g.a)\n(defn f [x] x)\n(defn g [] (f))\n(defn- unused [] 1)\n"
                       "src/g/ok.clj" "(ns g.ok)\n(defn h [] 1)\n"
                       "src/g/w.clj" "(ns g.w)\n(defn- quiet [] 1)\n"
                       "test/fixtures/bad.clj" "(ns fx.bad)\n(nope)\n"})]
    (spit (str root "/deps.edn") "{:paths [\"src\" \"test\"]}")
    (let [r (jolt-lsp.api/lint root {})]
      (testing "an error fails the gate; files are relative to the root"
        (is (:failed r))
        (is (= 2 (get-in r [:counts :error])))
        (is (some #(= "src/g/a.clj" (:file %)) (:findings r)))))
    (testing "excluded files are not linted"
      (let [r (jolt-lsp.api/lint root {:exclude ["test/fixtures"]})]
        (is (= 1 (get-in r [:counts :error])))
        (is (not-any? #(re-find #"fixtures" (:file %)) (:findings r)))))
    (testing "clj-kondo's :output :exclude-files is read too"
      (fs/create-dirs (str root "/.clj-kondo"))
      (spit (str root "/.clj-kondo/config.edn") "{:output {:exclude-files [\"test/fixtures\"]}}")
      (is (= 1 (get-in (jolt-lsp.api/lint root {}) [:counts :error]))))
    (testing "the fail level"
      (is (:failed (jolt-lsp.api/lint root {:fail-level :warning :exclude ["src/g/a.clj"]})))
      (is (not (:failed (jolt-lsp.api/lint root {:exclude ["src/g/a.clj"]})))))
    (testing "the text report"
      (is (re-find #"src/g/a.clj:3:\d+: error: g.a/f is called with 0 args but expects 1"
                   (jolt-lsp.cli/lint-text (jolt-lsp.api/lint root {})))))))

;; ---------------------------------------------------------------------------
;; false positives found surveying the libraries for the CI gate

(deftest gate-survey-false-positives
  (testing "data_readers files are data, not code"
    (let [root (project {"src/data_readers.clj" "{mulog/flake com.brunobonacci.mulog.flakes/read-method}\n"
                         "src/data_readers.cljc" "{time/date time-literals.read-write/date}\n"})]
      (is (empty? (diags root "src/data_readers.clj")))
      (is (empty? (diags root "src/data_readers.cljc")))))
  (testing "Jolt's built-in namespaces are known without a require"
    (is (empty? (local-diags* "(defn f [x] (clojure.math/floor x) (jolt.nrepl/start-server))"))))
  (testing ":use and :refer :all from a core namespace refer its vars"
    (is (empty? (filter #(= :unresolved-symbol (first %))
                        (local-diags* "(ns l.a (:use clojure.test))\n(deftest t (is (= 1 1)) (testing \"x\" 1))")))))
  (testing "a top-level alias names a namespace"
    (let [root (project {"src/b/p.clj" "(ns b.p)\n(defn median [xs] xs)\n"
                         "src/b/t.clj" "(ns b.t (:require [b.p]))\n(alias 'p 'b.p)\n(defn f [] (p/median [1]))\n"})]
      (is (empty? (diags root "src/b/t.clj")))))
  (testing "a file with no ns form is in user"
    (let [root (project {"src/script.clj" "(defn pem->der [f] f)\n(def root (pem->der 1))\n(prn root)\n"})]
      (is (empty? (filter #(= :unresolved-symbol (first %)) (diags root "src/script.clj"))))))
  (testing "a discarded form before an arity's params"
    (is (empty? (only :invalid-arity "(defn c ([p] (c p {})) (#_{:k 1} [p o] [p o]))\n(defn g [] (c 1 {}))"))))
  (testing "the keyword :/ reads"
    (let [root (project {"src/k/a.clj" "(ns k.a)\n(def ^:private ops {:+ \"+\" :/ \"/\"})\n(defn f [o] (ops o))\n"})]
      (is (empty? (diags root "src/k/a.clj"))))))

(deftest gate-survey-false-positives-2
  (testing "a record ctor from a :refer :all namespace"
    (let [root (project {"src/r/t.clj" "(ns r.t)\n(defrecord LogEntry [a b])\n"
                         "src/r/u.clj" "(ns r.u (:require [r.t :refer :all]))\n(defn f [] (->LogEntry 1 2))\n"})]
      (is (empty? (filter #(= :unresolved-symbol (first %)) (diags root "src/r/u.clj"))))))
  (testing "excluding a special form leaves it a special form"
    (is (empty? (filter #(= :unresolved-symbol (first %))
                        (local-diags* "(ns l.a (:refer-clojure :exclude [def and]))\n(def x 1)"))))))

(deftest with-redefs-and-binding-name-vars
  (let [root (project {"src/p/a.clj" "(ns p.a)\n(defn- f [] 1)\n(def ^:private ^:dynamic *d* 1)\n(defn g [] (f))\n"
                       "src/p/t.clj" "(ns p.t (:require [p.a :as a]))\n(defn t [] (with-redefs [a/f (fn [] 2)] (a/g)) (binding [a/*d* 2] (a/g)))\n"})]
    (is (empty? (filter #(= :private-call (first %)) (diags root "src/p/t.clj"))))))
