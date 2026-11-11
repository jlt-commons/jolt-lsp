;; Ported from xi, https://github.com/floscr/xi (commit 1701ddd),
;; src/xi/ext/clj_surgeon, MIT License, Copyright (c) 2026 Florian Schroedl.
;; Copied as written and adapted for samizdat, then moved to jolt-lsp;
;; the original notice is in docs/third-party/xi-LICENSE.
;; Pure alias resolution adapted from clj-surgeon source commit
;; affdfe3fd59a63b3fbbb50da16b82509fafea907, MIT License,
;; Copyright (c) 2026 Gene Kim; see docs/third-party/clj-surgeon-LICENSE.
;;
;; SPDX-License-Identifier: MIT

(ns jolt-lsp.surgeon.forms
  "Single source of truth for defining-form classification + per-form field
   extraction.

   Classification: a form's macro symbol is mapped to a *kind* keyword
   (:defn, :def, :defn-, etc.) so downstream ops (:deps, :topo, :ls-extract,
   :fix-declares) know what counts as a definition and what is private.

   Field extraction: for forms recognized via `.jolt-lsp.surgeon.edn`, the user
   declares `:fields` — a map of field-name → extractor function. Each
   extractor takes a zloc (pointing at the form list) and returns the value
   to emit in `:ls` output.

   Resolution order (4 tiers):
   1. Core forms — `defn`, `def`, `defmacro`, etc.
   2. Explicit aliases — `>defn`, `>defn-` (ecosystem macros)
   3. Project aliases — from `.jolt-lsp.surgeon.edn`
   4. Namespace-qualified split-on-/ — `mu/defn` → `defn` → tier 1-3"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [jolt-lsp.surgeon.fields :as fields]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]))

;; ============================================================
;; Core defining forms — these map to themselves
;; ============================================================

(def core-forms
  "Standard Clojure defining forms, keyed by their string name."
  {"def"         :def
   "defn"        :defn
   "defn-"       :defn-
   "defonce"     :defonce
   "defmacro"    :defmacro
   "defmethod"   :defmethod
   "defmulti"    :defmulti
   "defprotocol" :defprotocol
   "defrecord"   :defrecord
   "deftype"     :deftype
   "declare"     :declare
   "deftest"     :deftest})

;; ============================================================
;; Explicit aliases — ecosystem macros with non-standard names
;; ============================================================

(def explicit-aliases
  "Custom defining forms whose local name doesn't match a core form.
   Add ecosystem macros here (one line each)."
  {">defn"  :defn
   ">defn-" :defn-})

;; ============================================================
;; Project-local config — from .jolt-lsp.surgeon.edn
;; ============================================================

(def project-aliases
  "Singleton: map of macro-name-string -> spec map. Each spec is
   `{:kind <kw> :fields {field-key compiled-fn}}`. Populated by
   `init-from-file!`. Defaults empty."
  (atom {}))

(defn- infer-kind
  "If user didn't declare :kind, infer from name suffix.
   Trailing `-` → :defn-. Anything else → :defn."
  [type-str]
  (if (.endsWith ^String type-str "-")
    :defn-
    :defn))

;; ============================================================
;; User extractor fns
;;
;; A `(fn [z] ...)` extractor in .jolt-lsp/surgeon.edn is code, and jolt-lsp
;; does not evaluate project code on its own: bind *eval-extractor* to a
;; sandboxed evaluator (a fn of the form, returning a fn) to allow them. The
;; standard extractors in jolt-lsp.surgeon.fields need no evaluator.
;; ============================================================

(def ^:dynamic *eval-extractor* nil)

(defn- compile-field
  "Take an extractor form from `.jolt-lsp.surgeon.edn` and turn it into a real
   fn. If it's a symbol, resolve in stdlib. If it's a (fn ...) form,
   evaluate in SCI sandbox. Otherwise throw."
  [form macro-name field-key]
  (cond
    (fn? form) form

    (symbol? form)
    (or (get fields/public form)
        (throw (ex-info (str ".jolt-lsp.surgeon.edn: " macro-name " :fields "
                             field-key " — unknown extractor symbol: " form
                             "\n  Available: " (sort (keys fields/public)))
                        {:macro macro-name :field field-key :symbol form})))

    (and (seq? form) (= 'fn (first form)))
    (try (if *eval-extractor*
           (*eval-extractor* form)
           (throw (ex-info "fn extractors need jolt-lsp.surgeon.forms/*eval-extractor*" {})))
         (catch Exception e
           (throw (ex-info (str ".jolt-lsp.surgeon.edn: " macro-name " :fields "
                                field-key " — eval failed: " (.getMessage e))
                           {:macro macro-name :field field-key :form form} e))))

    :else
    (throw (ex-info (str ".jolt-lsp.surgeon.edn: " macro-name " :fields "
                         field-key " — value must be a stdlib symbol or a fn form, got: "
                         (pr-str form))
                    {:macro macro-name :field field-key :form form}))))

(defn- compile-spec
  "Turn a raw alias spec from `.jolt-lsp.surgeon.edn` into the runtime form.
   Returns {:kind kw :fields {k compiled-fn}}."
  [macro-name raw-value]
  (let [base (cond
               (keyword? raw-value) {:kind raw-value}
               (map? raw-value)     raw-value
               :else
               (throw (ex-info (str ".jolt-lsp.surgeon.edn: " macro-name
                                    " — value must be a kind keyword or a spec map")
                               {:macro macro-name :value raw-value})))
        kind (or (:kind base) (infer-kind macro-name))
        fields (when-let [fs (:fields base)]
                 (when-not (map? fs)
                   (throw (ex-info (str ".jolt-lsp.surgeon.edn: " macro-name
                                        " — :fields must be a map")
                                   {:macro macro-name :fields fs})))
                 (into {} (for [[fk fv] fs]
                            [fk (compile-field fv macro-name fk)])))]
    (cond-> {:kind kind}
      fields (assoc :fields fields))))

;; ============================================================
;; Config file resolution
;; ============================================================

(defn- find-config-file
  "Walk up from `start` (a file or dir path) looking for `.jolt-lsp.surgeon.edn`.
   Return the java.io.File for the first one found, or nil."
  [start]
  (let [start-file (some-> start io/file .getAbsoluteFile)
        start-dir (if (and start-file (.isDirectory start-file))
                    start-file
                    (some-> start-file .getParentFile))]
    (loop [dir start-dir]
      (when dir
        (let [cfg (io/file dir ".jolt-lsp.surgeon.edn")]
          (if (.exists cfg)
            cfg
            (recur (.getParentFile dir))))))))

(defn- read-config
  "Read + compile a `.jolt-lsp.surgeon.edn` file. Throws on malformed EDN or
   invalid extractor forms."
  [f]
  (let [parsed (try (edn/read-string (slurp f))
                    (catch Exception e
                      (throw (ex-info (str (.getPath f) ": invalid EDN — "
                                           (.getMessage e))
                                      {:file (.getPath f)} e))))
        aliases (:aliases parsed {})]
    (when-not (map? aliases)
      (throw (ex-info (str (.getPath f) ": :aliases must be a map")
                      {:file (.getPath f) :got aliases})))
    (into {} (for [[k v] aliases]
               (do (when-not (string? k)
                     (throw (ex-info (str (.getPath f)
                                          ": :aliases keys must be strings; got "
                                          (pr-str k))
                                     {:file (.getPath f) :key k})))
                   [k (compile-spec k v)])))))

(defn load-project-aliases
  "Find `.jolt-lsp.surgeon.edn` by walking up from `start` (a file or dir path).
   Return the compiled aliases map, or `{}` if no config found."
  [start]
  (if-let [f (find-config-file start)]
    (read-config f)
    {}))

(defn init-from-file!
  "Resolve `.jolt-lsp.surgeon.edn` near `file-path`, populate `project-aliases`.
   Call once per CLI invocation, from the top-level dispatch."
  [file-path]
  (reset! project-aliases (load-project-aliases file-path)))

;; ============================================================
;; Classification: the one function everything else calls
;; ============================================================

(defn- lookup-kind
  "Single-level lookup returning kind kw. Tiers: core forms, in-source
   explicit aliases, project aliases."
  [aliases s]
  (or (core-forms s)
      (explicit-aliases s)
      (:kind (aliases s))))

(defn- lookup-spec
  "Look up the full spec map. Core/explicit return {:kind kw} with no
   fields. Project entries return their full compiled spec."
  [aliases s]
  (or (when-let [k (core-forms s)] {:kind k})
      (when-let [k (explicit-aliases s)] {:kind k})
      (aliases s)))

(defn classify
  "Classify a form's type-str into a canonical kind (:defn, :defn-, :def, etc.)
   or nil if it's not a defining form.

   Resolution order:
   1. Exact match against core Clojure forms
   2. Exact match against explicit aliases (>defn, >defn-)
   3. Exact match against project aliases from .jolt-lsp.surgeon.edn
   4. Namespace-qualified: split on / and match local part against tiers 1-3
      (mu/defn -> 'defn' -> :defn, works for any alias)"
  [type-str]
  (when type-str
    (or (lookup-kind @project-aliases type-str)
        (when-let [idx (str/index-of type-str "/")]
          (lookup-kind @project-aliases (subs type-str (inc idx)))))))

(defn spec-with-project-aliases
  "Pure form-spec resolution against an explicit project-alias map."
  [project-aliases type-str]
  (when type-str
    (or (lookup-spec project-aliases type-str)
        (when-let [idx (str/index-of type-str "/")]
          (lookup-spec project-aliases (subs type-str (inc idx)))))))

(defn spec
  "Return the full spec map for a type-str — `{:kind <kw> :fields <map>}`
   or nil if not a defining form. Same tier resolution as `classify`."
  [type-str]
  (spec-with-project-aliases @project-aliases type-str))

;; ============================================================
;; Derived predicates — used across outline, analyze, extract
;; ============================================================

(defn defining-form?
  "Is this type-str a recognized defining form?"
  [type-str]
  (some? (classify type-str)))

(defn private-form?
  "Is this a private defining form (defn-, >defn-, mu/defn-, etc.)?"
  [type-str]
  (= :defn- (classify type-str)))

(defn has-arglists?
  "Does this form type have an arglist vector? (defn, defn-, >defn, mu/defn, etc.)
   Used by the default extract-arglist path — when a project alias provides
   custom :fields, the user controls arglist extraction directly."
  [type-str]
  (#{:defn :defn-} (classify type-str)))
