(ns jolt-lsp.sdiff.address-test
  "Naming a form by the shortest thing that is unambiguous
  (after sdiff's review find-form / find-change)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [jolt.fs :as fs]
            [jolt-lsp.sdiff.address :as address]))

(def src
  (str "(ns demo.core)\n"
       "\n"
       "(defn route [x]\n"
       "  (let [cap 10\n"
       "        n (inc x)]\n"
       "    (if (> n cap) :stop :go)))\n"
       "\n"
       "(defn helper\n"
       "  ([a] (helper a 1))\n"
       "  ([a b] (let [cap b] (+ a cap))))\n"
       "\n"
       "(defmethod run :fast [m] m)\n"))

(deftest a-form-by-its-id-or-its-unique-name
  (is (= [3 6] (:rows (address/locate src "defn route"))))
  (is (= [3 6] (:rows (address/locate src "route"))) "a bare name when it is unique")
  (is (= [12 12] (:rows (address/locate src "defmethod run :fast"))))
  (let [miss (address/locate src "routes")]
    (is (= :none (:error miss)))
    (is (some #{"defn route"} (:forms miss)) "a miss lists what there is")))

(deftest a-place-inside-a-form-by-its-path-or-a-unique-end-of-it
  (is (= [4 4] (:rows (address/locate src "route" "body › binding cap"))))
  (is (= [4 4] (:rows (address/locate src "route" "binding cap"))) "an end no sibling shares")
  (is (= [10 10] (:rows (address/locate src "helper" "arity 2 › let 2 › binding cap"))))
  (testing "an end two places share is ambiguous, and both are named"
    (let [r (address/locate src "helper" "vector 1")]
      (is (= :ambiguous (:error r)))
      (is (= 2 (count (:candidates r)))))))
