(ns jolt-lsp.core-info
  "Facts about the core namespaces, read from the running Jolt: arities
  from each fn's :arglists, :deprecated, :private, and which vars exist.
  clj-kondo ships a generated cache for this; Jolt can just look.

  Macros are left out of the arity facts: a core macro's :arglists is
  often documentation (`[name doc-string? attr-map? ...]`), not its params."
  (:require [jolt-lsp.arity :as arity]
            [jolt-lsp.resolve :as resolve]))

(defn- var-info [v]
  (let [m (meta v)
        al (:arglists m)]
    (cond-> {:macro (boolean (:macro m))
             :deprecated (boolean (:deprecated m))}
      (and (seq al) (not (:macro m)) (every? vector? al))
      (merge (arity/arity-info (map #(mapv str %) al))))))

(def ^:private table
  (delay
    (into {}
          (for [nsn resolve/core-namespaces
                :let [n (find-ns (symbol nsn))]
                :when n
                [s v] (ns-publics n)]
            [(str nsn "/" s) (var-info v)]))))

(defn lookup
  "Facts for a core fqn, or nil when no such public var is loaded."
  [fqn]
  (get @table fqn))

(defn loaded-ns?
  "Whether this core namespace is loaded, so a var missing from it is
  really missing."
  [nsn]
  (and (contains? resolve/core-namespaces nsn)
       (some? (find-ns (symbol nsn)))))
