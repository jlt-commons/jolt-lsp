(ns shop.web
  (:require [shop.core :as core]))

(defn page [items] (str (core/label (first items)) " " (core/total items)))
