(ns jolt-lsp.defcfn-proxy-test
  "jolt.ffi/defcfn defines a var and proxy methods bind their params and
  `this`. Each test builds a small project in a temp directory."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [jolt-lsp.db :as db]
            [jolt-lsp.diagnostics :as diagnostics]
            [jolt-lsp.query :as query]))

(defn- project! [files]
  (let [root (str (fs/canonicalize (fs/create-temp-dir {:prefix "jolt-lsp-defcfn"})))]
    (spit (str root "/deps.edn") "{:paths [\"src\"]}")
    (doseq [[p text] files]
      (fs/create-dirs (fs/parent (str root "/" p)))
      (spit (str root "/" p) text))
    root))

(defn- messages [d path]
  (set (map :message (diagnostics/file-diagnostics d path))))

(def ^:private ffi-src
  (str "(ns app.native\n"
       "  (:require [jolt.ffi :as ffi]))\n"
       "(ffi/defcfn ^:private unsetenv* \"unsetenv\" [:string] :int)\n"
       "(ffi/defcfn getpid* \"getpid\" [] :int)\n"
       "(defn clear! [k] (unsetenv* k) (getpid*))\n"))

(deftest aliased-defcfn-defines-a-var
  (let [root (project! {"src/app/native.clj" ffi-src})
        path (str root "/src/app/native.clj")
        d (db/build root)]
    (is (db/var-by-fqn d "app.native/unsetenv*"))
    (is (db/var-by-fqn d "app.native/getpid*"))
    (is (not-any? #(re-find #"Unresolved symbol: (unsetenv|getpid)" %) (messages d path))
        (pr-str (messages d path)))
    (testing "go-to-definition of a call finds the defcfn"
      ;; line 5: (defn clear! [k] (unsetenv* k) ...
      (let [defs (query/definition-at d path 5 28)]
        (is (= "app.native/unsetenv*" (:fqn (first defs))))))))

(deftest a-project-lint-as-wins-over-the-built-in-defcfn
  (let [root (project! {"src/app/native.clj" ffi-src})
        _ (fs/create-dirs (str root "/.clj-kondo"))
        _ (spit (str root "/.clj-kondo/config.edn")
                "{:lint-as {jolt.ffi/defcfn clojure.core/defn}}")
        d (db/build root)]
    ;; read as a defn, the C symbol string is a docstring
    (is (= "getpid" (:doc (db/var-by-fqn d "app.native/getpid*"))))))

(deftest proxy-methods-bind-params-and-this
  (let [src (str "(ns app.rd)\n"
                 "(defn make []\n"
                 "  (proxy [java.io.Reader] []\n"
                 "    (close [] (.toString this))\n"
                 "    (read\n"
                 "      ([] (.close this) -1)\n"
                 "      ([cbuf off len] (aget cbuf off) len))))\n")
        root (project! {"src/app/rd.clj" src})
        d (db/build root)
        ms (messages d (str root "/src/app/rd.clj"))]
    (is (not-any? #(re-find #"Unresolved symbol: (this|cbuf|off|len)" %) ms) (pr-str ms))
    (is (not-any? #(re-find #"Unused binding" %) ms) (pr-str ms))))

(deftest proxy-single-arity-and-unknown-symbols
  (let [src (str "(ns app.rd2)\n"
                 "(defn make [n]\n"
                 "  (proxy [java.io.Reader] [n]\n"
                 "    (skip [k] (+ k n mystery))))\n")
        root (project! {"src/app/rd2.clj" src})
        d (db/build root)
        ms (messages d (str root "/src/app/rd2.clj"))]
    (is (contains? ms "Unresolved symbol: mystery"))
    (is (not (contains? ms "Unresolved symbol: k")))
    (is (not (contains? ms "Unresolved symbol: n")))))

(deftest defcfn-through-refer-and-fully-qualified
  (let [referred (str "(ns app.r (:require [jolt.ffi :refer [defcfn]]))\n"
                      "(defcfn getuid* \"getuid\" [] :int)\n"
                      "(defn me [] (getuid*))\n")
        qualified (str "(ns app.q)\n"
                       "(jolt.ffi/defcfn getgid* \"getgid\" [] :int)\n"
                       "(defn me [] (getgid*))\n")
        root (project! {"src/app/r.clj" referred "src/app/q.clj" qualified})
        d (db/build root)]
    (is (db/var-by-fqn d "app.r/getuid*"))
    (is (db/var-by-fqn d "app.q/getgid*"))
    (is (not-any? #(re-find #"Unresolved symbol: get" %) (messages d (str root "/src/app/r.clj"))))
    (is (not-any? #(re-find #"Unresolved symbol: get" %) (messages d (str root "/src/app/q.clj"))))))
