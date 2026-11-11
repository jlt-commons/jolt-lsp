(ns app.core
  (:require [clojure.string :as string]
            [app.util :as util :refer [trimmed]]))

(declare later)

(defn greet
  "Say hi to name."
  [name]
  (str "hi " (trimmed name)))

(defn- secret
  []
  :app/secret)

(defn later
  []
  (secret))

(defmulti handle :type)

(defmethod handle :user
  [m]
  (:name m))

(defmethod handle :default
  [m]
  m)

(defprotocol Store
  (put [this k v])
  (get* [this k]))

(defrecord MapStore [state]
  Store
  (put [this k v] (assoc-in this [:state k] v))
  (get* [this k] (get-in this [:state k])))

(defn use-local
  [x]
  (let [y (inc x)
        {:keys [a]} {:a y}]
    (+ y a)))
