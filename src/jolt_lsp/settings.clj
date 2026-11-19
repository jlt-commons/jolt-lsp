(ns jolt-lsp.settings
  "Project settings from .jolt-lsp/config.edn, and the lint configuration,
  which follows clj-kondo's shape and is also read from
  .clj-kondo/config.edn (jolt-lsp's own file wins on conflicts):

    {:lint-as {app.macros/defthing clojure.core/defn}
     :linters {:unresolved-symbol {:exclude [match?            ; anywhere
                                             (app.dsl/law)       ; inside calls to it
                                             (app.dsl/rule [?x])]}  ; only these names
               :unresolved-namespace {:exclude [js]}}}"
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [jolt-lsp.kondo :as kondo]))

(def defaults
  {:kondo nil                 ; nil = on when a native clj-kondo is available
   :custom-linters []
   :clj-depend nil
   :format {}})

(def dsl-heads
  "Macros whose bodies name another namespace's vars as data: writ's spec
  forms, where a law calls the target's fns unqualified (so a name may
  shadow a core fn of another arity) and `calls` names private vars.
  Calls under them are not arity-checked, nor private uses flagged."
  #{"writ.spec/law" "writ.spec/calls" "writ.spec/ann" "writ.spec/graph"
    "writ.spec/refine"})

(defn- config-paths [root]
  [(str (fs/path root ".clj-kondo" "config.edn"))
   (str (fs/path root ".jolt-lsp" "config.edn"))])

(defn- read-edn [p]
  (try (when (fs/exists? p) (edn/read-string (slurp p))) (catch Throwable _ nil)))

(defn load-settings [root]
  (merge defaults (read-edn (second (config-paths root)))))

(defn kondo-enabled? [root]
  (let [v (:kondo (load-settings root))]
    (if (nil? v) (kondo/available?) (boolean v))))

(defn config-stamp
  "Changes whenever a config file the index depends on changes."
  [root]
  (mapv (fn [p] (try (str (fs/last-modified-time p)) (catch Throwable _ nil)))
        (config-paths root)))

(defn- deep-merge [a b]
  (if (and (map? a) (map? b)) (merge-with deep-merge a b) b))

(defn lint-config
  "{:lint-as {\"ns/macro\" \"defn\"}
    :exclude-symbols #{\"sym\" ...}
    :exclude-calls {\"ns/macro\" nil-or-#{\"name\" ...}}
    :exclude-namespaces #{\"ns\" ...}
    :skip-arity #{\"ns/macro\" ...}      ; [:linters :invalid-arity :skip-args]
    :levels {:code :off|:info|:warning|:error}}   ; [:linters <code> :level]
  merged from the kondo and jolt-lsp config files."
  [root]
  (let [cfg (reduce deep-merge {} (keep read-edn (config-paths root)))
        excludes (get-in cfg [:linters :unresolved-symbol :exclude])]
    {:lint-as (into {} (for [[k v] (:lint-as cfg)] [(str k) (name v)]))
     :exclude-symbols (set (for [x excludes :when (symbol? x)] (str x)))
     :exclude-calls (into {} (for [x excludes
                                   :when (and (seq? x) (symbol? (first x)))]
                               [(str (first x))
                                (when (vector? (second x)) (set (map str (second x))))]))
     :exclude-namespaces (set (map str (get-in cfg [:linters :unresolved-namespace :exclude])))
     :skip-arity (set (map str (get-in cfg [:linters :invalid-arity :skip-args])))
     :dsl-heads dsl-heads
     :levels (into {} (for [[code m] (:linters cfg)
                            :when (and (map? m) (keyword? (:level m)))]
                        [(keyword code) (:level m)]))}))
