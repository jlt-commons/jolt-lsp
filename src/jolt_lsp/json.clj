(ns jolt-lsp.json
  (:require [clojure.data.json :as json]
            [clojure.walk :as walk]))

(defn prepare [v]
  (walk/postwalk (fn [x]
                   (cond
                     (keyword? x) (name x)
                     (symbol? x) (str x)
                     :else x))
                 v))

(defn write [v]
  (json/write-str (prepare v)))

(defn read-json [s]
  (json/read-str s :key-fn keyword))
