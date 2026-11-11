(ns jolt-lsp.tree
  "Accessors over rewrite-clj nodes. Pure; no reader here."
  (:require [rewrite-clj.node :as n]))

(def ignore-tags #{:whitespace :newline :comment})

(defn tag [node]
  (n/tag node))

(defn text [node]
  (n/string node))

(def branch-tags
  #{:forms :list :vector :map :set :fn :quote :syntax-quote :unquote
    :unquote-splicing :deref :var :meta :reader-macro :namespaced-map})

(defn branch? [node]
  (contains? branch-tags (n/tag node)))

(defn children
  "Child nodes with whitespace, newlines and comments removed."
  [node]
  (vec (remove #(ignore-tags (n/tag %)) (n/children node))))

(defn all-children
  [node]
  (vec (n/children node)))

(defn pos [node]
  (select-keys (meta node) [:row :col :end-row :end-col]))

(defn node-range
  "1-based [row col end-row end-col], end-col exclusive."
  [node]
  (let [{:keys [row col end-row end-col]} (meta node)]
    [row col end-row end-col]))

(defn sexpr [node]
  (try (n/sexpr node) (catch Throwable _ nil)))

(defn keyword-node? [node]
  (or (= :keyword (n/tag node))
      (and (= :token (n/tag node))
           (keyword? (sexpr node)))))

(defn symbol-node? [node]
  (and (= :token (n/tag node))
       (symbol? (sexpr node))))

(defn string-node? [node]
  (or (= :string (n/tag node))
      (and (= :token (n/tag node))
           (string? (sexpr node)))))

(defn token-text [node]
  (text node))

(defn meta-form
  "The form a metadata node wraps, or the node itself."
  [node]
  (if (= :meta (n/tag node))
    (let [cs (children node)]
      (if (seq cs) (last cs) node))
    node))

(defn quote? [node]
  (= :quote (n/tag node)))

(defn uneval? [node]
  (= :uneval (n/tag node)))
