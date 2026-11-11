(ns jolt-lsp.tree
  "Accessors over jolt-lsp.parse nodes. Pure; no reader here."
  (:require [jolt-lsp.parse :as parse]))

(def ignore-tags #{:whitespace :newline :comment :comma})

(defn tag [node]
  (:tag node))

(defn text [node]
  (parse/node-text node))

(def branch-tags
  #{:forms :list :vector :map :set :fn :quote :syntax-quote :unquote
    :unquote-splicing :deref :var :meta :reader-macro :namespaced-map
    :uneval :eval})

(defn branch? [node]
  (contains? branch-tags (:tag node)))

(defn children
  "Child nodes with whitespace, newlines and comments removed."
  [node]
  (or (:kids node) []))

(defn all-children
  [node]
  (or (:all node) []))

(defn pos [node]
  (select-keys node [:row :col :end-row :end-col]))

(defn node-range
  "1-based [row col end-row end-col], end-col exclusive."
  [node]
  [(:row node) (:col node) (:end-row node) (:end-col node)])

(defn sexpr
  "The value a node reads as, or nil when it cannot be read alone."
  [node]
  (case (:kind node)
    :symbol (symbol (text node))
    (try (read-string (text node)) (catch Throwable _ nil))))

(defn keyword-node? [node]
  (= :keyword (:tag node)))

(defn symbol-node? [node]
  (= :symbol (:kind node)))

(defn string-node? [node]
  (= :string (:tag node)))

(defn token-text [node]
  (text node))

(defn meta-form
  "The form a metadata node wraps, through any stack of metadata, or the
  node itself."
  [node]
  (loop [node node]
    (if (and node (= :meta (:tag node)))
      (let [cs (children node)]
        (if (seq cs) (recur (last cs)) node))
      node)))

(defn quote? [node]
  (= :quote (:tag node)))

(defn uneval? [node]
  (= :uneval (:tag node)))
