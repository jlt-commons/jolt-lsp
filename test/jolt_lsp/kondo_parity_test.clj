(ns jolt-lsp.kondo-parity-test
  "Checks that follow clj-kondo's linters: arity, unresolved vars, private
  and deprecated calls, forward references, local findings, hooks, and the
  incremental index keeping other files' resolution current."
  (:require [babashka.fs :as fs]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [jolt-lsp.db :as db]
            [jolt-lsp.diagnostics :as diagnostics]
            [jolt-lsp.query :as query]))

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
