(ns jolt-lsp.kondo
  "Optional clj-kondo bridge. Uses a native binary when one is on PATH."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as string]
            [jolt-lsp.json :as json]))

(defn available? []
  (some? (fs/which "clj-kondo")))

(defn- absolute [path]
  (str (fs/absolutize path)))

(defn- finding->diag [f]
  {:file (absolute (:filename f))
   :row (or (:row f) 1)
   :col (or (:col f) 1)
   :end-row (or (:end-row f) (:row f) 1)
   :end-col (or (:end-col f) (:col f) 1)
   :severity (case (:level f)
               "error" :error
               "warning" :warning
               :info)
   :code (keyword (or (:type f) "kondo"))
   :message (or (:message f) "clj-kondo finding")
   :source :clj-kondo})

(defn lint
  "Lint these paths. Returns diagnostics, or [] when clj-kondo is absent or fails."
  [paths]
  (if-not (and (available?) (seq paths))
    []
    (try
      (let [cmd (into ["clj-kondo" "--lint"] paths)
            cmd (into cmd ["--config" "{:output {:format :json :canonical-paths true}}"])
            r (apply p/sh cmd)
            out (str (:out r))
            data (when-not (string/blank? out) (json/read-json out))]
        (mapv finding->diag (:findings data)))
      (catch Throwable _ []))))
