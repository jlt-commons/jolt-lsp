(ns jolt-lsp.layer
  "Namespace dependency rules, a small clj-depend-style check."
  (:require [clojure.string :as string]
            [jolt-lsp.db :as db]))

(defn matches? [pattern ns]
  (cond
    (string/ends-with? pattern "*")
    (string/starts-with? ns (subs pattern 0 (dec (count pattern))))
    :else (= pattern ns)))

(defn violations [rules edges]
  (vec
   (for [[from to] edges
         rule rules
         :when (and (matches? (:from rule) from)
                    (matches? (:to rule) to)
                    (not (:allow rule)))]
     [from to rule])))

(defn file-diagnostics [db path config]
  (let [file (db/file-by-path db path)
        ns (:ns file)
        rules (or (:rules config) [])
        edges (mapv (juxt :from-ns :to-ns) (or (db/ns-deps-of db ns) []))
        requires (or (db/ns-requires-of db ns) [])]
    (vec
     (for [[from to _rule] (violations rules edges)]
       (let [req (some #(when (= (:target %) to) %) requires)
             r [(or (:row req) 1) (or (:col req) 1)
                (or (:end-row req) 1) (or (:end-col req) 1)]]
         {:file path
          :row (nth r 0) :col (nth r 1)
          :end-row (nth r 2) :end-col (nth r 3)
          :severity :error
          :code :layer-violation
          :message (str "Namespace " from " must not depend on " to)
          :source :clj-depend})))))
