(ns jolt-lsp.core-info
  "Facts about the core namespaces, read from the running Jolt: arities
  from each fn's :arglists, :deprecated, :private, and which vars exist.
  clj-kondo ships a generated cache for this; Jolt can just look.

  Macros are left out of the arity facts: a core macro's :arglists is
  often documentation (`[name doc-string? attr-map? ...]`), not its params."
  (:require [clojure.string]
            [jolt-lsp.arity :as arity]
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

(def ^:private loadable
  (memoize
   (fn [nsn]
     (or (some? (find-ns (symbol nsn)))
         (try (require (symbol nsn)) true (catch Throwable _ false))))))

(defn builtin-ns?
  "A namespace Jolt itself provides (jolt.host, jolt.nrepl, clojure.math):
  loaded, or loadable from the runtime. A qualified name into it needs no
  require."
  [nsn]
  (boolean
   (and nsn (or (clojure.string/starts-with? nsn "jolt.")
                (clojure.string/starts-with? nsn "clojure."))
        (loadable nsn))))

(defn public-names
  "{name fqn} of a core or built-in namespace's public vars, for :use and
  :refer :all of it; nil for any other namespace."
  [nsn]
  (when (or (contains? resolve/core-namespaces nsn) (builtin-ns? nsn))
    (when-let [n (find-ns (symbol nsn))]
      (into {} (for [[s v] (ns-publics n)
                     :when (not (:private (meta v)))]
                 [(str s) (str nsn "/" s)])))))

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
