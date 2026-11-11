(ns jolt-lsp.format
  "Whole-file formatting through cljfmt, with the project's .cljfmt.edn."
  (:require [babashka.fs :as fs]
            [cljfmt.core :as cljfmt]
            [clojure.edn :as edn]))

(defn config [root]
  (try
    (let [p (fs/path root ".cljfmt.edn")]
      (if (fs/exists? p) (edn/read-string (slurp (str p))) {}))
    (catch Throwable _ {})))

(defn format-string [root source]
  (try
    {:ok true :text (cljfmt/reformat-string source (config root))}
    (catch Throwable e
      {:ok false :error (str e)})))

(defn format-file [root path]
  (let [source (slurp path)
        r (format-string root source)]
    (if (:ok r)
      {:ok true
       :path path
       :changed (not= source (:text r))
       :preview (:text r)}
      r)))
