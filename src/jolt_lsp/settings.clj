(ns jolt-lsp.settings
  "Project settings from .jolt-lsp/config.edn."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [jolt-lsp.kondo :as kondo]))

(def defaults
  {:kondo nil                 ; nil = on when a native clj-kondo is available
   :custom-linters []
   :clj-depend nil
   :format {}})

(defn load-settings [root]
  (try
    (let [p (fs/path root ".jolt-lsp" "config.edn")]
      (merge defaults
             (if (fs/exists? p) (edn/read-string (slurp (str p))) {})))
    (catch Throwable _ defaults)))

(defn kondo-enabled? [root]
  (let [v (:kondo (load-settings root))]
    (if (nil? v) (kondo/available?) (boolean v))))
