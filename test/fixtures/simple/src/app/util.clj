(ns app.util
  (:require [clojure.string :as string]))

(defn trimmed
  "Trim whitespace from both ends."
  [s]
  (string/trim s))

(defn- only-in-util
  []
  :util/hidden)

(defn public-util
  []
  :util/value)
