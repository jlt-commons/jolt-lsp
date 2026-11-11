(ns jolt-lsp.indent
  "A small indent-on-Enter helper: find the innermost open collection."
  (:require [clojure.string :as string]
            [jolt-lsp.text :as text]))

(defn- opener-at [source row col]
  (let [idx (text/position->offset source [row col])]
    (loop [i (dec idx) stack []]
      (if (neg? i)
        (peek stack)
        (let [c (nth source i)]
          (cond
            (or (= c \)) (= c \]) (= c \})) (recur (dec i) (conj stack c))
            (or (= c \() (= c \[) (= c \{)) (if (empty? stack)
                                              [c i]
                                              (recur (dec i) (pop stack)))
            :else (recur (dec i) stack)))))))

(defn indent [source row col]
  (if-let [[c i] (opener-at source row col)]
    (let [[r cc] (text/offset->position source i)
          line (nth (string/split-lines source) (dec r) "")
          text-before (subs line 0 (min (count line) (dec cc)))
          col (count text-before)]
      (cond
        (or (= c \[) (= c \{)) (apply str (repeat col " "))
        :else (apply str (repeat (+ col 2) " "))))
    ""))
