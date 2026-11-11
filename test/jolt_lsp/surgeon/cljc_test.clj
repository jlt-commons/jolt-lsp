(ns jolt-lsp.surgeon.cljc-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as reader-types]
            [jolt.fs :as fs]
            [jolt-lsp.surgeon.cljc.analyze :as analyze]
            [jolt-lsp.surgeon.cljc.merge :as merge]
            [jolt-lsp.surgeon.cljc.require-ops :as require-ops]
            [jolt-lsp.surgeon.cljc.split :as split]))

(def clj-source
  (str "(ns sample.shared\n"
       "  (:require [clojure.string :as str]\n"
       "            [clojure.java.io :as io]))\n\n"
       "(defn platform-file [x] (io/file x))\n\n"
       "(defn upper [x] (str/upper-case x))\n"))

(def cljs-source
  (str "(ns sample.shared\n"
       "  (:require [clojure.string :as str]\n"
       "            [goog.string :as gstr]))\n\n"
       "(defn platform-file [x] (gstr/htmlEscape x))\n\n"
       "(defn upper [x] (str/upper-case x))\n"))

(defn- parse-forms [source]
  (let [rdr (reader-types/string-push-back-reader source)]
    (->> (repeatedly #(reader/read {:eof ::end :read-cond :preserve} rdr))
         (take-while #(not= ::end %))
         vec)))

(defn- normalize-ns [form]
  (if (and (seq? form) (= 'ns (first form)))
    (let [[op name & clauses] form]
      (apply list op name
             (map (fn [clause]
                    (if (and (seq? clause) (= :require (first clause)))
                      (apply list :require (set (rest clause)))
                      clause))
                  clauses)))
    form))

(defn- normalized [source]
  (mapv normalize-ns (parse-forms source)))

(deftest merge-split-round-trips-and-reaches-a-fixed-point
  (let [cljc (merge/merge-files clj-source cljs-source)
        {:keys [clj cljs]} (split/split-file cljc)
        merged-again (merge/merge-files clj cljs)]
    (is (= (normalized clj-source) (normalized clj)))
    (is (= (normalized cljs-source) (normalized cljs)))
    (is (= (parse-forms cljc) (parse-forms merged-again)))
    (is (str/includes? cljc "#?@(:clj"))
    (is (str/includes? cljc "[goog.string :as gstr]"))))

(deftest platform-require-operations-preserve-platform-identity
  (let [cljc (merge/merge-files clj-source cljs-source)
        clj-only (require-ops/add-require
                  cljc {:platform :clj :ns 'clojure.set :as 'set})
        both (require-ops/add-require
              clj-only {:platform :cljc :ns 'clojure.walk :as 'walk})
        clj-split (:clj (split/split-file both))
        cljs-split (:cljs (split/split-file both))]
    (is (str/includes? clj-split "[clojure.set :as set]"))
    (is (not (str/includes? cljs-split "[clojure.set :as set]")))
    (is (str/includes? clj-split "[clojure.walk :as walk]"))
    (is (str/includes? cljs-split "[clojure.walk :as walk]"))
    (is (thrown-with-msg?
         Exception #"Alias"
         (require-ops/add-require both {:platform :clj :ns 'other.ns :as 'str}))))
  (let [npm (require-ops/add-require
             "(ns sample.empty)\n" {:platform :cljs :ns "react" :as 'react})]
    (is (str/includes? npm "\"react\""))
    (is (not (str/includes? (:clj (split/split-file npm)) "react")))))

(deftest analysis-reports-shared-platform-and-divergent-requires
  (let [pair (analyze/analyze-pair clj-source cljs-source)
        cljc (analyze/analyze-cljc (merge/merge-files clj-source cljs-source))]
    (is (= [{:ns "clojure.string" :as "str"}]
           (get-in pair [:requires :shared])))
    (is (= [{:ns "clojure.java.io" :as "io"}]
           (get-in pair [:requires :clj-only])))
    (is (= [{:ns "goog.string" :as "gstr"}]
           (get-in pair [:requires :cljs-only])))
    (is (= (:requires pair) (:requires cljc)))
    (is (= :cljc (:input cljc)))
    (is (= ["platform-file" "upper"] (mapv :name (:forms-clj pair))))))

(deftest malformed-cljc-input-is-refused
  (is (thrown-with-msg? Exception #"ns form"
                        (merge/merge-files "(def x 1)" "(def x 1)")))
  (is (thrown-with-msg? Exception #"ns names differ"
                        (merge/merge-files "(ns a)" "(ns b)")))
  (is (thrown-with-msg? Exception #"docstring"
                        (merge/merge-files "(ns a \"docs\")" "(ns a \"docs\")")))
  (is (thrown? Exception (split/split-file "(ns broken"))))

(deftest split-refuses-namespace-content-it-cannot-preserve
  (doseq [source ["(ns a \"docs\")"
                  "(ns a {:author \"team\"})"
                  "(ns a (:import java.util.UUID))"
                  "(ns a (:refer-clojure :exclude [map]))"]]
    (is (thrown? Exception (split/split-file source)) source))
  (is (thrown? Exception
               (merge/merge-files "(ns a (:require clojure.string))"
                                  "(ns a (:require clojure.string))")))
  (is (thrown? Exception (split/split-file "(ns a (:require clojure.string))"))))

(deftest split-and-merge-preserve-shared-default-branches-at-a-fixed-point
  (let [cljc (str "(ns sample.defaults\n"
                  "  (:require #?@(:default [[clojure.string :as str]])))\n\n"
                  "#?(:default (defn shared [] (str/upper-case \"ok\")))\n")
        {:keys [clj cljs]} (split/split-file cljc)
        merged (merge/merge-files clj cljs)
        split-again (split/split-file merged)]
    (is (str/includes? clj "[clojure.string :as str]"))
    (is (str/includes? cljs "[clojure.string :as str]"))
    (is (str/includes? clj "(defn shared []"))
    (is (str/includes? cljs "(defn shared []"))
    (is (= (normalized clj) (normalized (:clj split-again))))
    (is (= (normalized cljs) (normalized (:cljs split-again))))))

(deftest platform-branches-override-defaults-and-explicit-empty-branches-survive
  (let [cljc (str "(ns sample.asymmetric\n"
                  "  (:require #?@(:clj [] :default [[clojure.string :as str]])))\n\n"
                  "#?@(:clj [] :default [(defn shared [] :default)])\n"
                  "#?(:clj (def platform :clj) :default (def platform :default))\n")
        {:keys [clj cljs]} (split/split-file cljc)]
    (is (not (str/includes? clj "clojure.string")))
    (is (str/includes? cljs "[clojure.string :as str]"))
    (is (not (str/includes? clj "(defn shared")))
    (is (str/includes? cljs "(defn shared [] :default)"))
    (is (str/includes? clj "(def platform :clj)"))
    (is (str/includes? cljs "(def platform :default)"))
    (let [second-split (split/split-file (merge/merge-files clj cljs))]
      (is (= (normalized clj) (normalized (:clj second-split))))
      (is (= (normalized cljs) (normalized (:cljs second-split)))))))
