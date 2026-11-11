(ns jolt-lsp.resolve
  "Namespace-level symbol resolution over an analysis map.

  Locals are handled in the analysis walker; this answers the var/alias/
  refer/core question the index needs. The analysis map carries sets and
  maps built once per file (see jolt-lsp.db/ns-analysis), so a resolution
  is a few lookups."
  (:require [clojure.string :as string]))

(def core-namespaces
  #{"clojure.core" "clojure.string" "clojure.set" "clojure.edn"
    "clojure.walk" "clojure.pprint" "clojure.data" "clojure.java.io"
    "clojure.test" "clojure.zip" "clojure.repl" "clojure.template"})

(defn- qualified-name
  [analysis name i]
  (let [prefix (subs name 0 i)
        n (subs name (inc i))
        aliases (:aliases analysis)]
    (cond
      (contains? aliases prefix)
      {:kind :var :fqn (str (get aliases prefix) "/" n)}

      (or (= prefix (:ns-name analysis))
          (contains? (:requires analysis) prefix))
      {:kind :var :fqn (str prefix "/" n)}

      (contains? core-namespaces prefix)
      {:kind :core :fqn name}

      (contains? (:imports analysis) prefix)
      {:kind :class :fqn nil}

      :else
      {:kind :unresolved :fqn nil})))

(defn resolve-name
  "Resolve `name` as written in `current-ns`."
  [analysis current-ns name]
  (let [ns-name (:ns-name analysis)]
    (cond
      (or (nil? name) (= "" name))
      {:kind :unresolved :fqn nil}

      ;; "/" alone is clojure.core//
      (and (string/index-of name "/") (not= name "/"))
      (qualified-name analysis name (string/index-of name "/"))

      (contains? (:refers analysis) name)
      {:kind :var :fqn (get (:refers analysis) name)}

      (and ns-name (= ns-name current-ns) (contains? (:var-names analysis) name))
      {:kind :var :fqn (str ns-name "/" name)}

      (contains? (:refer-all analysis) name)
      {:kind :var :fqn (get (:refer-all analysis) name)}

      (contains? (:core analysis) name)
      {:kind :core :fqn (str "clojure.core/" name)}

      (contains? (:imports analysis) name)
      {:kind :class :fqn nil}

      :else
      {:kind :unresolved :fqn nil})))
