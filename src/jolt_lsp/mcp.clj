(ns jolt-lsp.mcp
  "MCP server over stdio, newline-delimited JSON-RPC 2.0."
  (:require [clojure.string :as string]
            [jolt-lsp.api :as api]
            [jolt-lsp.json :as json]
            [jolt-lsp.version :as version]))

(def instructions
  "Code intelligence and structural editing for Jolt (Clojure) projects:
navigation over a project index, diagnostics, refactors, and syntax repair.

- Positions are 1-based line/col. Paths may be relative to the project root.
- The index follows the disk: files you change with any tool are re-read on
  the next call. jolt_index refresh=true rebuilds from scratch.
- Edits preview by default and return a diff; pass apply=true to write. An
  edit that would leave a readable file unreadable is refused.
- Before changing a namespace: jolt_context. Before changing a public var:
  jolt_impact. Symbols may be fqns (app.core/run) or bare names.
- jolt_replace_form matches a form by what it READS as, not its text, so
  whitespace and line breaks in old_form do not matter.
- After writing Clojure, jolt_check finds unbalanced delimiters and unreadable
  text with line/col, and repairs a dropped trailing paren or an unclosed
  form the indentation places.")

(defn- obj [props & [required]]
  (cond-> {:type "object" :properties props}
    required (assoc :required required)))

(def ^:private root-prop {:type "string" :description "Project root (default: the server's directory)"})
(def ^:private file-prop {:type "string" :description "File path, absolute or relative to root"})
(def ^:private line-prop {:type "integer" :minimum 1 :description "1-based line"})
(def ^:private col-prop {:type "integer" :minimum 1 :description "1-based column"})
(def ^:private symbol-prop {:type "string" :description "A var: fqn (app.core/run) or bare name"})
(def ^:private aliases-prop {:type "array" :items {:type "string"}
                             :description "deps.edn aliases whose extra deps to include"})
(def ^:private apply-prop {:type "boolean" :description "Write the change (default: preview only)"})

(def tools
  [{:name "jolt_index"
    :description "Build or refresh the project index; returns counts and any files that do not read."
    :inputSchema (obj {:root root-prop :refresh {:type "boolean"}})}
   {:name "jolt_search"
    :description "Fuzzy-search var names across the project and its dependencies."
    :inputSchema (obj {:root root-prop :query {:type "string"}
                       :limit {:type "integer" :minimum 1}}
                      ["query"])}
   {:name "jolt_definition"
    :description "Where a var, local or keyword is defined: by symbol, or by file+line+col of a use."
    :inputSchema (obj {:root root-prop :symbol symbol-prop :file file-prop
                       :line line-prop :col col-prop})}
   {:name "jolt_references"
    :description "Every use of a var, plus its definition: by symbol, or by file+line+col."
    :inputSchema (obj {:root root-prop :symbol symbol-prop :file file-prop
                       :line line-prop :col col-prop})}
   {:name "jolt_hover"
    :description "Docstring, arglists and the full source of a var's definition."
    :inputSchema (obj {:root root-prop :symbol symbol-prop :file file-prop
                       :line line-prop :col col-prop})}
   {:name "jolt_symbols"
    :description "Outline a file: its definitions with ranges."
    :inputSchema (obj {:root root-prop :file file-prop} ["file"])}
   {:name "jolt_diagnostics"
    :description "Syntax errors, unresolved symbols, unused requires/refers/bindings/privates, duplicate requires (and clj-kondo when installed) for a file, or the whole project when no file is given."
    :inputSchema (obj {:root root-prop :file file-prop :all {:type "boolean"}
                       :kondo {:type "boolean"}})}
   {:name "jolt_call_hierarchy"
    :description "Direct and transitive callers or callees of a var."
    :inputSchema (obj {:root root-prop :symbol symbol-prop
                       :direction {:type "string" :enum ["callers" "callees"]}}
                      ["symbol"])}
   {:name "jolt_impact"
    :description "What a change to a var can break: every transitive caller with its distance."
    :inputSchema (obj {:root root-prop :symbol symbol-prop} ["symbol"])}
   {:name "jolt_deps"
    :description "Transitive namespace dependencies or dependents, or every require cycle."
    :inputSchema (obj {:root root-prop :ns {:type "string"}
                       :direction {:type "string" :enum ["dependencies" "dependents" "cycles"]}})}
   {:name "jolt_deps_list"
    :description "The libraries the project depends on (transitively), with version and the directory their files are in."
    :inputSchema (obj {:root root-prop :pattern {:type "string" :description "Regex on lib names"}
                       :aliases aliases-prop})}
   {:name "jolt_deps_grep"
    :description "Regex search across the source files of the project's dependencies. Returns lib, file, line and text; pass file to jolt_deps_read."
    :inputSchema (obj {:root root-prop :pattern {:type "string"}
                       :library {:type "string" :description "group/artifact, group, or artifact"}
                       :glob {:type "string" :description "File filter, e.g. *.clj or *.{clj,cljc}"}
                       :case_insensitive {:type "boolean"}
                       :files_only {:type "boolean"}
                       :limit {:type "integer" :minimum 1 :description "Max results (default 50)"}
                       :aliases aliases-prop}
                      ["pattern"])}
   {:name "jolt_deps_read"
    :description "Read a file inside one of the project's dependencies, optionally a line range. Paths outside the dependencies are refused."
    :inputSchema (obj {:root root-prop :file file-prop
                       :library {:type "string"}
                       :path {:type "string" :description "Path relative to the library, with library"}
                       :offset {:type "integer" :minimum 1 :description "First line (default 1)"}
                       :limit {:type "integer" :minimum 1 :description "Number of lines"}
                       :aliases aliases-prop})}
   {:name "jolt_form"
    :description "The innermost form at a position, or every top-level form without one."
    :inputSchema (obj {:root root-prop :file file-prop :line line-prop :col col-prop
                       :mode {:type "string" :enum ["enclosing" "top"]}}
                      ["file"])}
   {:name "jolt_context"
    :description "One-call pack for a namespace: vars, requires/aliases, dependencies, dependents, diagnostics."
    :inputSchema (obj {:root root-prop :ns {:type "string"} :file file-prop})}
   {:name "jolt_check"
    :description "Check Clojure text for unbalanced delimiters, unterminated strings and unreadable tokens, with line/col. Repairs what is safe: a dropped trailing paren, an extra trailing closer, a closer the indentation implies. fix=true writes the repair to the file."
    :inputSchema (obj {:root root-prop :file file-prop
                       :code {:type "string" :description "Text to check instead of a file"}
                       :fix {:type "boolean"}})}
   {:name "jolt_rename"
    :description "Rename the var, local or keyword at a position across the project."
    :inputSchema (obj {:root root-prop :file file-prop :line line-prop :col col-prop
                       :to {:type "string"} :apply apply-prop}
                      ["file" "line" "col" "to"])}
   {:name "jolt_clean_ns"
    :description "Remove unused and duplicate requires and unused refers. Side-effect requires are kept."
    :inputSchema (obj {:root root-prop :file file-prop :apply apply-prop} ["file"])}
   {:name "jolt_add_require"
    :description "Require the namespace of a symbol (ns/name, or a bare var name to look up). Returns how to write the symbol afterwards."
    :inputSchema (obj {:root root-prop :file file-prop :symbol {:type "string"}
                       :alias {:type "string"} :apply apply-prop}
                      ["file" "symbol"])}
   {:name "jolt_replace_form"
    :description "Replace the form that reads equal to old_form, at any depth, ignoring whitespace and formatting. Strings and comments never match. Empty new_text deletes it. Several matches are listed: pick with occurrence or pass all."
    :inputSchema (obj {:root root-prop :file file-prop
                       :old_form {:type "string" :description "One Clojure form"}
                       :new_text {:type "string" :description "Continuation lines relative to column 1, or at their final indentation"}
                       :occurrence {:type "integer" :minimum 1}
                       :all {:type "boolean"}
                       :apply apply-prop}
                      ["file" "old_form" "new_text"])}
   {:name "jolt_insert_form"
    :description "Insert new_text after (or before) the form that reads equal to an anchor, indented to it; with no anchor, append to the file."
    :inputSchema (obj {:root root-prop :file file-prop
                       :after {:type "string"} :before {:type "string"}
                       :occurrence {:type "integer" :minimum 1}
                       :new_text {:type "string"} :apply apply-prop}
                      ["file" "new_text"])}
   {:name "jolt_refactor"
    :description "Structural refactor at a position: sort-requires, cycle-keyword (::k <-> :ns/k), inline-symbol (a single-use let binding), destructure-keys ((:k m) uses -> {:keys [k]}), restructure-keys (the reverse, in a let), move-form (to target_file)."
    :inputSchema (obj {:root root-prop
                       :kind {:type "string"
                              :enum ["sort-requires" "cycle-keyword" "inline-symbol"
                                     "destructure-keys" "restructure-keys" "move-form"]}
                       :file file-prop :line line-prop :col col-prop
                       :target_file {:type "string"} :apply apply-prop}
                      ["kind" "file"])}
   {:name "jolt_structural_diff"
    :description "What changed in a Clojure file, by form and by where inside it: behaviour changes told apart from renames, extractions, moves, wrapping and comment- or whitespace-only edits, with changed forms grouped by the calls between them. Old side: old (text), old_file, or the file at git rev (default HEAD); new side: new (text) or the file on disk."
    :inputSchema (obj {:root root-prop :file file-prop
                       :old {:type "string"} :old_file {:type "string"}
                       :rev {:type "string" :description "git revision for the old side (default HEAD)"}
                       :new {:type "string"}}
                      ["file"])}
   {:name "jolt_rename_namespace"
    :description "Rename a namespace (or a namespace prefix): its files move to the paths the new name implies, and every ns form, require and qualified reference in the project follows."
    :inputSchema (obj {:root root-prop :from {:type "string"} :to {:type "string"} :apply apply-prop}
                      ["from" "to"])}
   {:name "jolt_fix_declares"
    :description "Remove declares a file no longer needs by moving each definition above its first use; a declare that breaks a real cycle stays."
    :inputSchema (obj {:root root-prop :file file-prop :apply apply-prop} ["file"])}
   {:name "jolt_extract"
    :description "Move named top-level forms of a file, with the private helpers only they use, into a namespace (created at the path its name implies). Returns the files that still call a moved var, from the index, so their requires can be updated."
    :inputSchema (obj {:root root-prop :file file-prop
                       :forms {:type "array" :items {:type "string"}}
                       :to_ns {:type "string"} :apply apply-prop}
                      ["file" "forms" "to_ns"])}
   {:name "jolt_format"
    :description "Format a whole file with cljfmt and the project's .cljfmt.edn."
    :inputSchema (obj {:root root-prop :file file-prop :apply apply-prop} ["file"])}
   {:name "jolt_batch"
    :description "Run several read-only tool calls in one round trip; each op's root defaults to this call's."
    :inputSchema (obj {:root root-prop
                       :ops {:type "array"
                             :items (obj {:tool {:type "string"} :args {:type "object"}})}}
                      ["ops"])}])

(def ^:private read-only
  #{"jolt_index" "jolt_search" "jolt_definition" "jolt_references" "jolt_hover"
    "jolt_symbols" "jolt_diagnostics" "jolt_call_hierarchy" "jolt_impact"
    "jolt_deps" "jolt_deps_list" "jolt_deps_grep" "jolt_deps_read" "jolt_form" "jolt_context" "jolt_structural_diff"})

(defn- norm-args [m]
  (into {}
        (map (fn [[k v]] [(keyword (string/replace (name k) "_" "-")) v]))
        m))

(defn call-tool [name args]
  (let [args (norm-args args)
        root (:root args)
        a (dissoc args :root)]
    (case name
      "jolt_index" (do (when (:refresh a) (api/refresh! root))
                       (api/index-status root))
      "jolt_search" (api/search root a)
      "jolt_definition" (api/definition root a)
      "jolt_references" (api/references root a)
      "jolt_hover" (api/hover root a)
      "jolt_symbols" (api/document-symbols root a)
      "jolt_diagnostics" (api/diagnostics* root a)
      "jolt_call_hierarchy" (api/call-hierarchy root a)
      "jolt_impact" (api/impact root a)
      "jolt_deps" (api/deps root a)
      "jolt_deps_list" (api/deps-list root a)
      "jolt_deps_grep" (api/deps-grep root a)
      "jolt_deps_read" (api/deps-read root a)
      "jolt_form" (api/form root a)
      "jolt_context" (api/context root a)
      "jolt_check" (api/check root a)
      "jolt_rename" (api/rename* root a)
      "jolt_clean_ns" (api/clean-ns* root a)
      "jolt_add_require" (api/add-require* root a)
      "jolt_replace_form" (api/replace-form* root a)
      "jolt_insert_form" (api/insert-form* root a)
      "jolt_refactor" (api/refactor* root a)
      "jolt_format" (api/format* root a)
      "jolt_structural_diff" (api/structural-diff root a)
      "jolt_rename_namespace" (api/rename-namespace* root a)
      "jolt_fix_declares" (api/fix-declares* root a)
      "jolt_extract" (api/extract* root a)
      "jolt_batch" {:results
                    (mapv (fn [{:keys [tool args]}]
                            (if (contains? read-only tool)
                              (try (call-tool tool (merge (when root {:root root}) args))
                                   (catch Throwable e {:ok false :error (str e)}))
                              {:ok false :error (str tool " is not a read-only tool")}))
                          (:ops a))}
      {:ok false :error (str "unknown tool " name)})))

(defn- error-result? [v]
  (and (map? v) (false? (:ok v))))

(defn- result [id value]
  {:jsonrpc "2.0" :id id :result value})

(defn- error [id code message]
  {:jsonrpc "2.0" :id id :error {:code code :message message}})

(def ^:private protocol-versions #{"2024-11-05" "2025-03-26" "2025-06-18"})

(defn handle [msg]
  (let [method (:method msg)
        id (:id msg)]
    (case method
      "initialize" (result id {:protocolVersion (let [v (get-in msg [:params :protocolVersion])]
                                                  (if (protocol-versions v) v "2024-11-05"))
                               :capabilities {:tools {}}
                               :serverInfo {:name "jolt-lsp" :version version/version}
                               :instructions instructions})
      "notifications/initialized" nil
      "tools/list" (result id {:tools tools})
      "tools/call" (let [params (:params msg)
                         name (:name params)
                         args (or (:arguments params) {})]
                     (try
                       (let [v (call-tool name args)]
                         (result id {:content [{:type "text" :text (json/write v)}]
                                     :isError (error-result? v)}))
                       (catch Throwable e
                         (result id {:content [{:type "text"
                                                :text (json/write {:ok false :error (str e)})}]
                                     :isError true}))))
      "ping" (result id {})
      (when id (error id -32601 (str "unknown method " method))))))

(defn -main [& _args]
  (loop [line (read-line)]
    (when line
      (when-not (string/blank? line)
        (try
          (let [msg (json/read-json line)
                resp (handle msg)]
            (when resp
              (println (json/write resp))
              (flush)))
          (catch Throwable e
            (println (json/write (error nil -32700 (str e))))
            (flush))))
      (recur (read-line))))
  (System/exit 0))
