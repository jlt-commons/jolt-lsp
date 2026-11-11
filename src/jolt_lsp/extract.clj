(ns jolt-lsp.extract
  "The query surface over one file's analysis.

  The reader and walker live in jolt-lsp.analyze; these fns are the stable,
  writ-checked accessors the engine and the specs use."
  (:refer-clojure :exclude [ns-name])
  (:require [jolt-lsp.analyze :as analyze]))

(defn ns-name
  [source path]
  (:ns (analyze/analyze source path)))

(defn var-names
  [source path]
  (mapv :name (:vars (analyze/analyze source path))))

(defn private-var-names
  [source path]
  (->> (:vars (analyze/analyze source path))
       (filter :private)
       (mapv :name)))

(defn alias-map
  [source path]
  (get-in (analyze/analyze source path) [:ns-meta :aliases]))

(defn refer-map
  [source path]
  (get-in (analyze/analyze source path) [:ns-meta :refers]))

(defn required-namespaces
  [source path]
  (set (get-in (analyze/analyze source path) [:ns-meta :requires])))

(defn keyword-fqns
  [source path]
  (vec (distinct (map :fqn (:keywords (analyze/analyze source path))))))
