;; Portions adapted from clj-kondo (https://github.com/clj-kondo/clj-kondo),
;; Copyright (c) Michiel Borkent, distributed under the Eclipse Public
;; License 1.0: analyze-arity and arity-match? (clj-kondo.impl.analyzer),
;; show-arities and arity-error (clj-kondo.impl.linters). See NOTICE.
(ns jolt-lsp.arity
  "Arity facts and the arity check. Pure.

  A params vector is given as the text of its items, so `[x & more]` is
  [\"x\" \"&\" \"more\"] and a destructured param is one item."
  (:require [clojure.string :as string]))

(defn param-arity
  "{:fixed-arity n :min-arity n-or-nil :varargs bool} for one params vector."
  [params]
  (reduce (fn [acc p]
            (if (= "&" p)
              (reduced (assoc acc :min-arity (:fixed-arity acc) :varargs true))
              (update acc :fixed-arity inc)))
          {:fixed-arity 0 :min-arity nil :varargs false}
          params))

(defn arity-info
  "{:fixed-arities #{n ...} :varargs-min-arity n-or-nil} for a fn's
  params vectors, one per body."
  [bodies]
  (let [as (map param-arity bodies)]
    {:fixed-arities (set (keep #(when-not (:varargs %) (:fixed-arity %)) as))
     :varargs-min-arity (some #(when (:varargs %) (:min-arity %)) as)}))

(defn conflicting-arities
  "Fixed arities more than one body has, in order."
  [bodies]
  (->> (map param-arity bodies)
       (remove :varargs)
       (map :fixed-arity)
       frequencies
       (keep (fn [[n c]] (when (> c 1) n)))
       sort
       vec))

(defn recur-arity
  "How many arguments a recur to this body takes: a rest param is one."
  [params]
  (let [a (param-arity params)]
    (if (:varargs a) (inc (:fixed-arity a)) (:fixed-arity a))))

(defn fn-literal-arity
  "The arity of #(...) from the % names in its body."
  [names]
  (let [n (reduce max 0 (keep (fn [s]
                                (cond (= "%" s) 1
                                      (re-matches #"%[1-9][0-9]*" s) (parse-long (subs s 1))
                                      :else nil))
                              names))]
    (if (some #{"%&"} names)
      {:fixed-arities #{} :varargs-min-arity n}
      {:fixed-arities #{n} :varargs-min-arity nil})))

(defn matches?
  "Whether a call with n args fits."
  [{:keys [fixed-arities varargs-min-arity]} n]
  (boolean (or (contains? fixed-arities n)
               (and varargs-min-arity (>= n varargs-min-arity)))))

(defn known?
  "Whether there is anything to check against."
  [{:keys [fixed-arities varargs-min-arity]}]
  (boolean (or (seq fixed-arities) varargs-min-arity)))

(defn show-arities
  "\"2\", \"0, 1 or 3\", \"1, 2 or more\"."
  [{:keys [fixed-arities varargs-min-arity]}]
  (let [fas (vec (sort fixed-arities))
        max-fixed (peek fas)
        arities (if varargs-min-arity
                  (if (= max-fixed varargs-min-arity)
                    fas (conj fas varargs-min-arity))
                  fas)]
    (cond varargs-min-arity (str (string/join ", " arities) " or more")
          (= 1 (count fas)) (str (first fas))
          :else (str (string/join ", " (pop arities)) " or " (peek arities)))))

(defn arity-message [arity fn-name called-with]
  (str fn-name " is called with " called-with
       (if (= 1 called-with) " arg" " args")
       " but expects " (show-arities arity)))
