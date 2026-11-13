(ns acme.widget
  "A widget that spins.")

(defn spin
  "Spin n times."
  [n]
  (repeat n :spin))

(defn Spin-twice [] (spin 2))
