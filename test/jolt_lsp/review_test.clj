(ns jolt-lsp.review-test
  "Regressions for the review fixes: analyzer false positives, clean-ns and
  add-require safety, edit plumbing, the LSP frame reader, and the agent
  API's freshness and edit checks. Each test builds its own project in a
  temp directory."
  (:require [babashka.fs :as fs]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [jolt-lsp.api :as api]
            [jolt-lsp.db :as db]
            [jolt-lsp.diagnostics :as diagnostics]
            [jolt-lsp.edit.add-require :as add-require]
            [jolt-lsp.edit.clean-ns :as clean-ns]
            [jolt-lsp.edit.rename :as rename]
            [jolt-lsp.edit.vedit :as vedit]
            [jolt-lsp.graph :as graph]
            [jolt-lsp.lsp.rpc :as rpc]
            [jolt-lsp.read-impl :as read]
            [jolt-lsp.refactor :as refactor]
            [jolt-lsp.text :as text]
            [jolt-lsp.text.index :as tix]
            [jolt-lsp.tree :as tree]))

(defn- project!
  "A temp project holding `files` ({\"src/a/b.clj\" text}). Returns its root."
  [files]
  (let [root (str (fs/canonicalize (fs/create-temp-dir {:prefix "jolt-lsp-review"})))]
    (spit (str root "/deps.edn") "{:paths [\"src\" \"test\"]}")
    (doseq [[p text] files]
      (fs/create-dirs (fs/parent (str root "/" p)))
      (spit (str root "/" p) text))
    root))

(defn- codes [root path]
  (let [d (db/build root)]
    (set (map (juxt :code :message)
              (diagnostics/file-diagnostics d (str root "/" path))))))

(def ^:private spec-src "(ns app.spec)\n(defn helper [x] x)\n")

(deftest no-false-positives
  (let [root (project!
              {"src/app/spec.clj" spec-src
               "src/app/side.clj" "(ns app.side)\n"
               "src/app/core.clj"
               (str "(ns app.core\n"
                    "  (:require [app.spec :as s]\n"
                    "            [app.side]\n"
                    "            [clojure.string :as str]))\n"
                    "(defn private-name [] 1)\n"
                    "(defn kw [m] (::s/name m))\n"
                    "(defn f [a b]\n"
                    "  (try (str/join a)\n"
                    "    (catch Exception e\n"
                    "      (case b foo 1 (bar baz) 2 3))))\n"
                    "(defn g [^String x] (Math/abs (.length x)))\n"
                    "(defmacro m [x] `(let [y# ~x] (undefined-in-template y#)))\n"
                    "(defn h [{:keys [k] :or {k 1}} {v :v}] [k v])\n")})
        cs (codes root "src/app/core.clj")]
    (is (empty? cs) (pr-str cs))))

(deftest real-findings-are-found
  (let [root (project!
              {"src/app/spec.clj" spec-src
               "src/app/core.clj"
               (str "(ns app.core\n"
                    "  (:require [app.spec :as s]\n"
                    "            [app.spec :as dup]\n"
                    "            [clojure.set :refer [union difference]]))\n"
                    "(defn- secret [] 1)\n"
                    "(defn f [x unused] (union (s/helper x) #{}))\n"
                    "(defn g [] (nope))\n")})
        cs (set (map first (codes root "src/app/core.clj")))]
    (is (contains? cs :duplicate-require))
    (is (contains? cs :unused-referred-var))
    (is (contains? cs :unused-binding))
    (is (contains? cs :unused-private-var))
    (is (contains? cs :unresolved-symbol))))

(deftest deftest-and-comment-bodies-count-as-usage
  (let [root (project!
              {"src/app/spec.clj" spec-src
               "test/app/core_test.clj"
               (str "(ns app.core-test\n"
                    "  (:require [clojure.test :refer [deftest is]]\n"
                    "            [app.spec :as sp]\n"
                    "            [app.other :as o]))\n"
                    "(deftest t (sp/helper 1) (is true))\n"
                    "(comment (o/thing))\n")})]
    (is (empty? (codes root "test/app/core_test.clj")))))

(deftest reader-conditionals-use-the-jolt-branch
  (let [root (project!
              {"src/app/spec.clj" spec-src
               "src/app/c.cljc"
               (str "(ns app.c (:require #?(:clj [app.spec :as s])))\n"
                    "(defn f [] #?(:jolt (s/helper 1) :cljs (js/alert 1)))\n")})]
    (is (empty? (codes root "src/app/c.cljc")))))

(deftest syntax-errors-are-diagnosed
  (let [root (project! {"src/app/broken.clj"
                        "(ns app.broken)\n(defn f [x]\n  (let [y x\n    y))\n(defn g [] 2)\n"})
        d (db/build root)
        ds (diagnostics/file-diagnostics d (str root "/src/app/broken.clj"))]
    (is (some #(= :syntax-error (:code %)) ds))
    (testing "the forms around the broken one still index"
      (is (db/var-by-fqn d "app.broken/g")))))

(deftest tolerant-parse-keeps-later-forms
  (doseq [src ["(ns a)\n(defn f [] (foo)\n(defn g [] 2)\n"
               "(ns a)\n(defn f [] (foo]))\n(defn g [] 2)\n"
               "(ns a)\n(def x 1.2.3)\n(defn g [] 2)\n"
               "(ns a)\n(defn g [] 2)\n1R0"]]
    (let [forms (map tree/text (:forms (read/parse-text src)))]
      (is (some #{"(defn g [] 2)"} forms) src))))

(deftest clean-ns-keeps-what-is-used
  (let [root (project!
              {"src/app/spec.clj" spec-src
               "src/app/side.clj" "(ns app.side)\n"
               "src/app/m.clj"
               (str "(ns app.m\n"
                    "  (:require [app.spec :as s]\n"
                    "            [clojure.string :as str :refer [join trim]]\n"
                    "            app.side\n"
                    "            [clojure.set :as set]))\n"
                    "(defn f [m] [(::s/k m) (join \",\" [])])\n")})
        path (str root "/src/app/m.clj")
        r (clean-ns/plan (db/build root) path)]
    (is (= (str "(ns app.m\n"
                "  (:require [app.spec :as s]\n"
                "            [clojure.string :as str :refer [join]]\n"
                "            app.side))\n"
                "(defn f [m] [(::s/k m) (join \",\" [])])\n")
           (:preview r)))))

(deftest add-require-without-a-require-clause
  (let [root (project! {"src/app/spec.clj" spec-src
                        "src/app/a.clj" "(ns app.a)\n\n(defn f [] 1)\n"})
        d (db/build root)
        path (str root "/src/app/a.clj")
        r (add-require/plan d {:path path :symbol "helper"})]
    (is (:ok r))
    (is (= "spec/helper" (:use r)))
    (is (string/includes? (:preview r) "(ns app.a\n  (:require [app.spec :as spec]))"))
    (is (re-find #"no require needed"
                 (:reason (add-require/plan d {:path (str root "/src/app/spec.clj")
                                               :symbol "app.spec/helper"}))))))

(deftest edits-carry-start-row
  (let [e (vedit/range->edit "abc\ndef" [2 1 2 2] "X")]
    (is (= [2 1 2 2] ((juxt :start-row :start-col :end-row :end-col) e)))))

(deftest graph-is-linear
  (let [edges (vec (for [i (range 5000)] [i (inc i)]))
        t0 (System/currentTimeMillis)]
    (is (= 5001 (count (graph/reachable edges 0))))
    (is (< (- (System/currentTimeMillis) t0) 5000))))

(deftest lsp-frames-count-bytes
  (let [body "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"x\",\"params\":{\"s\":\"héllo→😀\"}}"
        bytes (count (.getBytes body "UTF-8"))
        input (str "Content-Length: " bytes "\r\n\r\n" body "Content-Length: 2\r\n\r\n{}")]
    (with-in-str input
      (is (= "héllo→😀" (get-in (rpc/read-message) [:params :s])))
      (is (= {} (rpc/read-message))))))

(deftest inline-refuses-params
  (let [root (project! {"src/app/a.clj" "(ns app.a)\n(defn f [a b] (inc a))\n"})
        d (db/build root)
        path (str root "/src/app/a.clj")
        l (some #(when (= "a" (:name %)) %)
                (db/local-defs-in-file d (:id (db/file-by-path d path))))]
    (is (false? (:ok (refactor/inline-symbol d path (:row l) (:col l)))))))

(deftest keyword-rename-follows-destructuring
  (let [root (project! {"src/app/a.clj"
                        "(ns app.a)\n(defn f [{:keys [user]}] (inc user))\n(defn g [] (f {:user 1}))\n"})
        d (db/build root)
        path (str root "/src/app/a.clj")
        r (rename/rename d {:path path :row 3 :col 16 :to ":account"})]
    (is (:ok r))
    (is (= "(ns app.a)\n(defn f [{:keys [account]}] (inc account))\n(defn g [] (f {:account 1}))\n"
           (get (:preview r) path)))))

(deftest api-follows-the-disk-and-guards-edits
  (let [root (project! {"src/app/spec.clj" spec-src
                        "src/app/a.clj" "(ns app.a\n  (:require [app.spec :as s]))\n(defn f [] (s/helper 1))\n"})]
    (api/refresh! root)
    (testing "relative paths and fresh reads after an outside write"
      (Thread/sleep 1100)
      (spit (str root "/src/app/spec.clj") "(ns app.spec)\n(defn helper [x] x)\n(defn added [] 2)\n")
      (Thread/sleep 300)
      (is (= "app.spec/added" (:fqn (first (api/definition root {:symbol "added"})))))
      (is (= "app.spec/helper"
             (:fqn (first (api/definition root {:file "src/app/a.clj" :line 3 :col 15}))))))
    (testing "an edit that breaks a readable file is refused"
      (let [r (api/replace-form* root {:file "src/app/a.clj" :old-form "(s/helper 1)"
                                       :new-text "(s/helper 1" :apply true})]
        (is (false? (:ok r)))
        (is (string/includes? (slurp (str root "/src/app/a.clj")) "(s/helper 1))"))))
    (testing "structural replace at depth, applied and re-indexed"
      (let [r (api/replace-form* root {:file "src/app/a.clj" :old-form "(s/helper   1)"
                                       :new-text "(s/added)" :apply true})]
        (is (:ok r))
        (is (string/includes? (get-in r [:diff (str root "/src/app/a.clj")]) "+(defn f [] (s/added))"))
        (is (seq (api/references root {:symbol "app.spec/added"})))))
    (testing "insert after a form, and append"
      (is (:ok (api/insert-form* root {:file "src/app/a.clj" :after "(defn f [] (s/added))"
                                       :new-text "(defn g [] 3)" :apply true})))
      (is (:ok (api/insert-form* root {:file "src/app/a.clj" :new-text "(defn h [] 4)" :apply true})))
      (is (= "(ns app.a\n  (:require [app.spec :as s]))\n(defn f [] (s/added))\n\n(defn g [] 3)\n\n(defn h [] 4)\n"
             (slurp (str root "/src/app/a.clj")))))
    (testing "check repairs a dropped paren"
      (let [r (api/check root {:code "(defn f [x]\n  (inc x)"})]
        (is (= :repaired (:status r)))
        (is (= "(defn f [x]\n  (inc x))" (:code r)))))))

(deftest kondo-style-lint-config
  (let [files {"src/app/dsl.clj" "(ns app.dsl)\n(defn check [& xs] xs)\n(defmacro defthing [& body] `(do ~@body))\n"
               "src/app/core.clj"
               (str "(ns app.core (:require [app.dsl :as dsl]))\n"
                    "(dsl/defthing f [x] (inc x))\n"
                    "(defn g [] (f 1))\n"
                    "(defn h [] (dsl/check ?a ?b))\n"
                    "(defn k [] (match? 1))\n")}
        root (project! files)]
    (testing "without config: the dsl args and match? are unresolved"
      (is (contains? (set (map second (codes root "src/app/core.clj")))
                     "Unresolved symbol: ?a")))
    (fs/create-dirs (str root "/.clj-kondo"))
    (spit (str root "/.clj-kondo/config.edn")
          "{:lint-as {app.dsl/defthing clojure.core/defn}
            :linters {:unresolved-symbol {:exclude [(app.dsl/check [?a]) match?]}}}")
    (let [d (db/build root)
          cs (set (map :message (diagnostics/file-diagnostics d (str root "/src/app/core.clj"))))]
      (testing "lint-as makes defthing define a var with params"
        (is (db/var-by-fqn d "app.core/f"))
        (is (not (contains? cs "Unresolved symbol: x"))))
      (testing "excludes: a name everywhere, names inside one call"
        (is (not (contains? cs "Unresolved symbol: match?")))
        (is (not (contains? cs "Unresolved symbol: ?a")))
        (is (contains? cs "Unresolved symbol: ?b"))))))

(deftest cached-text-index-agrees-with-text
  (let [s "(ns a)\n\n(def 😀x \"é→\")\r\n  tail\n"]
    (doseq [o (range (inc (count s)))]
      (is (= (text/offset->position s o) (tix/offset->position s o)) o)
      (is (= (text/utf8-offset s o) (tix/utf8-offset s o)) o))
    (doseq [row (range 0 8) col (range 0 14)]
      (is (= (text/position->offset s [row col]) (tix/position->offset s [row col])))
      (is (= (text/codepoint->utf16-col s row col) (tix/codepoint->utf16-col s row col))))))
