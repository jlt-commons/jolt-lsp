(ns jolt-lsp.mcp
  "MCP server over stdio, newline-delimited JSON-RPC 2.0."
  (:require [clojure.string :as string]
            [jolt-lsp.api :as api]
            [jolt-lsp.json :as json]))

(def tools
  [{:name "jolt_index"
    :description "Build or refresh the project index and return counts."
    :inputSchema {:type "object"
                  :properties {:root {:type "string" :description "Project root"}
                               :refresh {:type "boolean"}}}}
   {:name "jolt_search"
    :description "Fuzzy-search project vars, namespaces, and keywords."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"}
                               :query {:type "string"}
                               :limit {:type "integer" :minimum 1}}
                  :required ["query"]}}
   {:name "jolt_definition"
    :description "Find the definition of a symbol by fqn or by file:line:col (1-based)."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"}
                               :symbol {:type "string"}
                               :file {:type "string"}
                               :line {:type "integer" :minimum 1}
                               :col {:type "integer" :minimum 1}}}}
   {:name "jolt_references"
    :description "Find every reference to an fqn, including its definition."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"}
                               :symbol {:type "string"}}
                  :required ["symbol"]}}
   {:name "jolt_hover"
    :description "Docstring, arglists, and source for a symbol."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"}
                               :symbol {:type "string"}
                               :file {:type "string"}
                               :line {:type "integer"}
                               :col {:type "integer"}}}}
   {:name "jolt_symbols"
    :description "Outline a file."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"} :file {:type "string"}}
                  :required ["file"]}}
   {:name "jolt_diagnostics"
    :description "Built-in findings for a file or the whole project."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"}
                               :file {:type "string"}
                               :all {:type "boolean"}
                               :kondo {:type "boolean"}}}}
   {:name "jolt_call_hierarchy"
    :description "Callers or callees with call-site edges."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"}
                               :symbol {:type "string"}
                               :direction {:type "string" :enum ["callers" "callees"]}}
                  :required ["symbol"]}}
   {:name "jolt_impact"
    :description "Transitive callers affected by a change."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"} :symbol {:type "string"}}
                  :required ["symbol"]}}
   {:name "jolt_deps"
    :description "Namespace dependencies or dependents."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"}
                               :ns {:type "string"}
                               :direction {:type "string"
                                           :enum ["dependencies" "dependents"]}}
                  :required ["ns"]}}
   {:name "jolt_form"
    :description "The enclosing form at a position, or every top-level form."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"}
                               :file {:type "string"}
                               :line {:type "integer"}
                               :col {:type "integer"}
                               :mode {:type "string" :enum ["enclosing" "top"]}}
                  :required ["file"]}}
   {:name "jolt_context"
    :description "A namespace pack: vars, dependencies, dependents, diagnostics."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"} :ns {:type "string"}
                               :file {:type "string"}}}}
   {:name "jolt_rename"
    :description "Rename a var, local, or keyword. Previews by default."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"}
                               :file {:type "string"}
                               :line {:type "integer"}
                               :col {:type "integer"}
                               :to {:type "string"}
                               :apply {:type "boolean"}}
                  :required ["file" "line" "col" "to"]}}
   {:name "jolt_clean_ns"
    :description "Drop unused and duplicate requires. Previews by default."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"} :file {:type "string"}
                               :apply {:type "boolean"}}
                  :required ["file"]}}
   {:name "jolt_add_require"
    :description "Add a require for a symbol. Previews by default."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"} :file {:type "string"}
                               :symbol {:type "string"} :alias {:type "string"}
                               :apply {:type "boolean"}}
                  :required ["file" "symbol"]}}
   {:name "jolt_replace_form"
    :description "Structurally replace a top-level form. Previews by default."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"} :file {:type "string"}
                               :old_form {:type "string"} :new_text {:type "string"}
                               :apply {:type "boolean"}}
                  :required ["file" "old_form" "new_text"]}}
   {:name "jolt_insert_form"
    :description "Structurally insert after a form. Previews by default."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"} :file {:type "string"}
                               :after {:type "string"} :new_text {:type "string"}
                               :apply {:type "boolean"}}
                  :required ["file" "after" "new_text"]}}
   {:name "jolt_refactor"
    :description "Structural refactor: sort-requires, cycle-keyword, inline-symbol, destructure-keys, move-form. Previews by default."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"}
                               :kind {:type "string"
                                      :enum ["sort-requires" "cycle-keyword" "inline-symbol"
                                             "destructure-keys" "restructure-keys" "move-form"]}
                               :file {:type "string"}
                               :line {:type "integer"}
                               :col {:type "integer"}
                               :target_file {:type "string"}
                               :apply {:type "boolean"}}
                  :required ["kind" "file"]}}
   {:name "jolt_format"
    :description "Format a whole file with cljfmt. Previews by default."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"} :file {:type "string"}
                               :apply {:type "boolean"}}
                  :required ["file"]}}
   {:name "jolt_batch"
    :description "Run several read-only tool calls in one round trip."
    :inputSchema {:type "object"
                  :properties {:root {:type "string"}
                               :ops {:type "array"
                                     :items {:type "object"
                                             :properties {:tool {:type "string"}
                                                          :args {:type "object"}}}}}}}])

(defn- norm-args [m]
  (into {}
        (map (fn [[k v]] [(keyword (string/replace (name k) "_" "-")) v]))
        m))

(defn- call-tool [name args]
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
      "jolt_form" (api/form root a)
      "jolt_context" (api/context root a)
      "jolt_rename" (api/rename* root a)
      "jolt_clean_ns" (api/clean-ns* root a)
      "jolt_add_require" (api/add-require* root a)
      "jolt_replace_form" (api/replace-form* root a)
      "jolt_insert_form" (api/insert-form* root a)
      "jolt_refactor" (api/refactor* root a)
      "jolt_format" (api/format* root a)
      "jolt_batch" {:results (mapv (fn [op] (call-tool (:tool op) (:args op))) (:ops a))}
      {:ok false :error (str "unknown tool " name)})))

(defn- result [id value]
  {:jsonrpc "2.0" :id id :result value})

(defn- error [id code message]
  {:jsonrpc "2.0" :id id :error {:code code :message message}})

(defn handle [msg]
  (let [method (:method msg)
        id (:id msg)]
    (case method
      "initialize" (result id {:protocolVersion "2024-11-05"
                               :capabilities {:tools {}}
                               :serverInfo {:name "jolt-lsp" :version "0.1.0"}})
      "notifications/initialized" nil
      "tools/list" (result id {:tools tools})
      "tools/call" (let [params (:params msg)
                         name (:name params)
                         args (or (:arguments params) {})]
                     (try
                       (result id {:content [{:type "text"
                                              :text (json/write (call-tool name args))}]
                                   :isError false})
                       (catch Throwable e
                         (result id {:content [{:type "text"
                                                :text (json/write {:ok false :error (str e)})}]
                                     :isError true}))))
      "ping" (result id {})
      "shutdown" (result id nil)
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
