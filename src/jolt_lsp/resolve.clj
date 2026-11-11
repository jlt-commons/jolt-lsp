(ns jolt-lsp.resolve
  "Namespace-level symbol resolution over an analysis map.

  Locals are handled in the analysis walker; this answers the var/alias/
  refer/core question the index needs."
  (:require [clojure.string :as string]))

(def core-namespaces
  #{"clojure.core" "clojure.string" "clojure.set" "clojure.edn"
    "clojure.walk" "clojure.pprint" "clojure.data" "clojure.java.io"})

(defn- known-var?
  [analysis ns name]
  (some (fn [v]
          (and (= (:ns v) ns) (= (:name v) name)))
        (:vars analysis)))

(defn- slash-index [s]
  (string/index-of s "/"))

(defn- qualified-name
  [analysis current-ns name]
  (let [i (slash-index name)
        prefix (subs name 0 i)
        n (subs name (inc i))
        aliases (:aliases analysis)
        requires (set (:requires analysis))
        ns-name (:ns-name analysis)]
    (cond
      (contains? aliases prefix)
      {:kind :var :fqn (str (get aliases prefix) "/" n)}

      (= prefix ns-name)
      {:kind :var :fqn (str prefix "/" n)}

      (contains? requires prefix)
      {:kind :var :fqn (str prefix "/" n)}

      (contains? core-namespaces prefix)
      {:kind :core :fqn name}

      :else
      {:kind :unresolved :fqn nil})))

(defn resolve-name
  "Resolve `name` as written in `current-ns`."
  [analysis current-ns name]
  (let [ns-name (:ns-name analysis)
        refers (:refers analysis)
        core (set (:core analysis))]
    (cond
      (or (nil? name) (= "" name))
      {:kind :unresolved :fqn nil}

      (some? (slash-index name))
      (qualified-name analysis current-ns name)

      (contains? refers name)
      {:kind :var :fqn (get refers name)}

      (and ns-name (= ns-name current-ns) (known-var? analysis ns-name name))
      {:kind :var :fqn (str ns-name "/" name)}

      (contains? core name)
      {:kind :core :fqn (str "clojure.core/" name)}

      :else
      {:kind :unresolved :fqn nil})))
