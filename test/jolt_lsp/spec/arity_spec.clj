(ns jolt-lsp.spec.arity-spec
  "Arity facts and the arity check, after clj-kondo.

    params  -> {:fixed-arity n} or {:min-arity n :varargs true}
    bodies  -> {:fixed-arities #{n ...} :varargs-min-arity n-or-nil}
    a call with n args matches when n is a fixed arity or at least the
    varargs minimum"
  (:require [writ.spec :refer [spec ann graph law refine]]))

(spec jolt-lsp.arity {:require :tested})

(refine Params [p (List String)] true)

(refine ParamArity [a {:fixed-arity Nat
                       :min-arity (Opt Nat)
                       :varargs Bool}] true)

(refine Arity [a {:fixed-arities (Set Nat)
                  :varargs-min-arity (Opt Nat)}] true)

(ann param-arity [Params -> ParamArity])
(ann arity-info [(List Params) -> Arity])
(ann fn-literal-arity [(List String) -> Arity])
(ann matches? [Arity Nat -> Bool])
(ann show-arities [Arity -> String])
(ann arity-message [Arity String Nat -> String])
(ann known? [Arity -> Bool])
(ann conflicting-arities [(List Params) -> (List Nat)])
(ann recur-arity [Params -> Nat])

(graph checking
  {:start :params
   :states {:params (List Params)
            :arity Arity
            :verdict Bool
            :text String
            :dupes (List Nat)}
   :edges {:params {[arity-info] #{:arity}
                    [conflicting-arities] #{:dupes}}
           :arity {[matches? Nat] #{:verdict}
                   [show-arities] #{:text}
                   [arity-message String Nat] #{:text}}}
   :final [:verdict :text :dupes]})

(law fixed-params-count
  (= {:fixed-arity 2 :min-arity nil :varargs false} (param-arity ["x" "y"])))

(law varargs-params
  (= {:fixed-arity 1 :min-arity 1 :varargs true} (param-arity ["x" "&" "more"])))

(law destructured-params-count-once
  (= 2 (:fixed-arity (param-arity ["[a b]" "{:keys [c]}"]))))

(law multi-arity-info
  (= {:fixed-arities #{0 1} :varargs-min-arity 2}
     (arity-info [[] ["a"] ["a" "b" "&" "r"]])))

(law fixed-arity-matches-exactly
  (forall [n Nat]
    (and (matches? (arity-info [(vec (repeat n "x"))]) n)
         (not (matches? (arity-info [(vec (repeat n "x"))]) (inc n))))))

(law varargs-match-at-least-min
  (forall [n Nat, k Nat]
    (matches? (arity-info [(conj (vec (repeat n "x")) "&" "r")]) (+ n k))))

(law below-varargs-min-fails
  (not (matches? (arity-info [["a" "b" "&" "r"]]) 1)))

(law shows-single
  (= "2" (show-arities {:fixed-arities #{2} :varargs-min-arity nil})))

(law shows-choice
  (= "0, 1 or 3" (show-arities {:fixed-arities #{0 1 3} :varargs-min-arity nil})))

(law shows-or-more
  (= "0, 2 or more" (show-arities {:fixed-arities #{0} :varargs-min-arity 2})))

(law shows-or-more-merged
  (= "1 or more" (show-arities {:fixed-arities #{1} :varargs-min-arity 1})))

(law message-wording
  (= "app.a/two is called with 1 arg but expects 2"
     (arity-message {:fixed-arities #{2} :varargs-min-arity nil} "app.a/two" 1)))

(law message-plural
  (= "f is called with 3 args but expects 0 or 1"
     (arity-message {:fixed-arities #{0 1} :varargs-min-arity nil} "f" 3)))

(law fn-literal-highest-arg
  (= {:fixed-arities #{3} :varargs-min-arity nil} (fn-literal-arity ["%" "%3"])))

(law fn-literal-percent-is-one
  (= {:fixed-arities #{1} :varargs-min-arity nil} (fn-literal-arity ["%"])))

(law fn-literal-rest
  (= {:fixed-arities #{} :varargs-min-arity 1} (fn-literal-arity ["%1" "%&"])))

(law fn-literal-none
  (= {:fixed-arities #{0} :varargs-min-arity nil} (fn-literal-arity [])))

(law duplicate-arity-reported
  (= [1] (conflicting-arities [["a"] ["b"] []])))

(law distinct-arities-not-reported
  (empty? (conflicting-arities [["a"] [] ["a" "&" "r"]])))

(law recur-counts-rest-as-one
  (and (= 2 (recur-arity ["a" "b"]))
       (= 2 (recur-arity ["a" "&" "r"]))))

(law message-names-call-and-expectation
  (forall [nm String, n Nat]
    (let [a {:fixed-arities #{2} :varargs-min-arity nil}]
      (= (str nm " is called with " n (if (= 1 n) " arg" " args") " but expects 2")
         (arity-message a nm n)))))

(law known-when-any-arity
  (and (known? {:fixed-arities #{0} :varargs-min-arity nil})
       (known? {:fixed-arities #{} :varargs-min-arity 0})
       (not (known? {:fixed-arities #{} :varargs-min-arity nil}))))
