(ns jolt-lsp.settings
  "Project settings from .jolt-lsp/config.edn, and the lint configuration,
  which follows clj-kondo's shape and is also read from
  .clj-kondo/config.edn (jolt-lsp's own file wins on conflicts):

    {:lint-as {app.macros/defthing clojure.core/defn}
     :linters {:unresolved-symbol {:exclude [match?            ; anywhere
                                             (app.dsl/law)       ; inside calls to it
                                             (app.dsl/rule [?x])]}  ; only these names
               :unresolved-namespace {:exclude [js]}}}

  As clj-kondo does, configs under .clj-kondo's :config-paths, its
  `<org>/<lib>/` and `imports/<org>/<lib>/` dirs, and the
  `clj-kondo.exports/<org>/<lib>/` dirs dependencies ship are read too,
  under the project's own; that is where :hooks usually come from."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [jolt-lsp.kondo :as kondo]
            [jolt-lsp.paths :as paths]))

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

(defn- lib-dirs
  "<base>/<org>/<lib>/ and <base>/imports/<org>/<lib>/ dirs with a config.edn."
  [base]
  (when (fs/directory? base)
    (->> (concat (fs/glob base "*/*/config.edn") (fs/glob base "imports/*/*/config.edn"))
         (map #(str (fs/parent %)))
         (remove #(re-find #"/\.cache/" %))
         sort)))

(defn- local-kondo-dirs
  "The project's clj-kondo config dirs, lowest precedence first: imported
  and :config-paths dirs, then .clj-kondo itself."
  [root]
  (let [base (str (fs/path root ".clj-kondo"))
        cfg (read-edn (str base "/config.edn"))
        cps (for [p (:config-paths cfg)
                  :let [d (str (fs/path base (str p)))]
                  :when (fs/directory? d)]
              d)]
    (vec (distinct (concat (lib-dirs base) cps (when (fs/directory? base) [base]))))))

(defonce ^:private export-cache (atom {}))

(defn- export-dirs
  "The clj-kondo.exports/<org>/<lib>/ dirs of the project's dependencies,
  cached per root until deps.edn changes."
  [root]
  (let [stamp (try (str (fs/last-modified-time (fs/path root "deps.edn"))) (catch Throwable _ nil))
        hit (get @export-cache root)]
    (if (= stamp (:stamp hit))
      (:dirs hit)
      (let [dirs (vec (distinct
                       (for [lib (try (paths/dep-libraries root) (catch Throwable _ []))
                             ;; the exports sit on the library's classpath:
                             ;; its root for a JAR, a :paths entry for a checkout
                             :let [cp (distinct (concat [""] (:paths (read-edn (str (fs/path (:dir lib) "deps.edn"))))
                                                        ["src" "resources"]))]
                             p cp
                             :let [base (str (fs/path (:dir lib) (str p) "clj-kondo.exports"))]
                             :when (fs/directory? base)
                             f (sort (map str (fs/glob base "*/*/config.edn")))]
                         (str (fs/parent f)))))]
        (swap! export-cache assoc root {:stamp stamp :dirs dirs})
        dirs))))

(defn- hook-files [dirs]
  (sort (for [d dirs
              f (fs/glob d "**.{clj,cljc,clj_kondo}")]
          (str f))))

(defn config-stamp
  "Changes whenever a config file the index depends on changes, hook code
  under the project's .clj-kondo included."
  [root]
  (let [dirs (local-kondo-dirs root)]
    (mapv (fn [p] (try (str p "@" (fs/last-modified-time p)) (catch Throwable _ nil)))
          (concat (config-paths root)
                  (map #(str % "/config.edn") dirs)
                  (hook-files dirs)))))

(defn- deep-merge [a b]
  (if (and (map? a) (map? b)) (merge-with deep-merge a b) b))

(defn- hooks-config
  "{:analyze-call {\"ns/macro\" hook-sym} :macroexpand {...} :dirs [...]
  :config cfg :stamp n}, or nil when no hook is configured or :hooks is
  false in .jolt-lsp/config.edn."
  [root cfg dirs]
  (let [h (:hooks cfg)
        strs (fn [m] (into {} (for [[k v] m] [(str k) v])))]
    (when (and (map? h) (or (seq (:analyze-call h)) (seq (:macroexpand h)))
               (not (false? (:hooks (load-settings root)))))
      {:analyze-call (strs (:analyze-call h))
       :macroexpand (strs (:macroexpand h))
       :dirs (vec (reverse dirs))
       :config cfg
       :stamp (hash [h (mapv #(try (slurp %) (catch Throwable _ "")) (hook-files dirs))])})))

(defn lint-config
  "{:lint-as {\"ns/macro\" \"defn\"}
    :exclude-symbols #{\"sym\" ...}
    :exclude-calls {\"ns/macro\" nil-or-#{\"name\" ...}}
    :exclude-namespaces #{\"ns\" ...}
    :skip-arity #{\"ns/macro\" ...}      ; [:linters :invalid-arity :skip-args]
    :levels {:code :off|:info|:warning|:error}   ; [:linters <code> :level]
    :hooks {...}}                       ; see hooks-config
  merged from the kondo and jolt-lsp config files. With :deps?, the
  configs dependencies export are read too (indexing wants them; a
  diagnostics request only needs the levels)."
  ([root] (lint-config root {}))
  ([root {:keys [deps?]}]
  (let [dirs (concat (when deps? (export-dirs root)) (local-kondo-dirs root))
        cfg (reduce deep-merge {}
                    (keep read-edn (concat (for [d dirs
                                                 :when (not= d (str (fs/path root ".clj-kondo")))]
                                             (str d "/config.edn"))
                                           (config-paths root))))
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
                        [(keyword code) (:level m)]))
     ;; {"ns/name" {:arities {n {:args [...] :ret t}}}}: clj-kondo's
     ;; [:linters :type-mismatch :namespaces]
     :type-specs (into {} (for [[nsn fns] (get-in cfg [:linters :type-mismatch :namespaces])
                                [nm spec] fns]
                            [(str nsn "/" nm) spec]))
     :hooks (hooks-config root cfg dirs)})))
