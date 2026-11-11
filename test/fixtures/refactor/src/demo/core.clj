(ns demo.core
  (:require [clojure.set :as set]
            [clojure.string :as string]))

(defn use-inline
  [x]
  (let [y (inc x)]
    (* 2 y)))

(defn use-destructure
  [m]
  (let [v m]
    (+ (:a v) (:b v))))

(defn restructure
  [m]
  (let [a (:a m)
        b (:b m)]
    (+ a b)))

(defn auto-keyword
  []
  ::thing)
