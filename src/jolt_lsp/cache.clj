(ns jolt-lsp.cache
  "Analysis cache: a project's file analyses keyed by path and content hash."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]))

(def version 1)

(defn cache-file [root]
  (str (fs/path root ".jolt-lsp" "cache.edn")))

(defn read-cache [root]
  (try
    (let [m (edn/read-string (slurp (cache-file root)))]
      (when (= version (:version m)) (:entries m)))
    (catch Throwable _ {})))

(defn write-cache! [root entries]
  (try
    (fs/create-dirs (fs/path root ".jolt-lsp"))
    (spit (cache-file root)
          (pr-str {:version version
                   :root root
                   :entries (into {}
                                  (map (fn [e]
                                         [(:path e)
                                          {:hash (:hash e)
                                           :external (:external e)
                                           :analysis (:analysis e)}]))
                                  entries)}))
    entries
    (catch Throwable _ entries)))
