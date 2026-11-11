(ns app.data
  (:require [clojure.edn :as edn]))

(def default-config
  {:app/port 8080
   :app/host "localhost"})

(defn read-config
  [s]
  (merge default-config (edn/read-string s)))
