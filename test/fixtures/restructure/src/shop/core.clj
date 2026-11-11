(ns shop.core
  (:require [clojure.string :as str]))

(declare total)

(defn- cents [x] (* 100 x))

(defn price [item] (cents (:price item)))

(defn label [item] (str/upper-case (:name item)))

(defn receipt [items] (str "total " (total items)))

(defn total [items] (reduce + (map price items)))
