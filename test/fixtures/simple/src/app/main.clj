(ns app.main
  (:require [app.core :as core]))

(defn -main
  [& args]
  (println (core/greet (first args))))
