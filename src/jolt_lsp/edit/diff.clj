(ns jolt-lsp.edit.diff
  "A compact unified-style diff of range edits, for agents to review."
  (:require [clojure.string :as string]
            [jolt-lsp.edit.vedit :refer [apply-edits]]
            [jolt-lsp.text :as text]))

(defn- line-of [starts offset]
  ;; 0-based line holding offset
  (dec (count (take-while #(<= % offset) starts))))

(defn diff
  "A unified-style diff of `edits` against `source`: one hunk per run of
  touched lines, no context lines. Line numbers are 1-based."
  [source edits]
  (let [new (apply-edits source edits)
        old-lines (string/split source #"\n" -1)
        new-lines (string/split new #"\n" -1)
        old-starts (text/line-starts source)
        new-starts (text/line-starts new)
        spans (second
               (reduce (fn [[delta out] e]
                         (let [ns (+ (:start-offset e) delta)
                               ne (+ ns (count (:new-text e)))]
                           [(+ delta (- (count (:new-text e))
                                        (- (:end-offset e) (:start-offset e))))
                            (conj out [(line-of old-starts (:start-offset e))
                                       (line-of old-starts (:end-offset e))
                                       (line-of new-starts ns)
                                       (line-of new-starts ne)])]))
                       [0 []]
                       (sort-by :start-offset edits)))
        hunks (reduce (fn [acc [o1 o2 _ n2 :as s]]
                        (let [[p1 p2 q1 q2] (peek acc)]
                          (if (and p2 (<= o1 p2))
                            (conj (pop acc) [p1 (max p2 o2) q1 (max q2 n2)])
                            (conj acc s))))
                      []
                      spans)]
    (string/join
     "\n"
     (for [[o1 o2 n1 n2] hunks]
       (str "@@ -" (inc o1) "," (inc (- o2 o1)) " +" (inc n1) "," (inc (- n2 n1)) " @@\n"
            (string/join "\n" (map #(str "-" %) (subvec old-lines o1 (inc o2))))
            "\n"
            (string/join "\n" (map #(str "+" %) (subvec new-lines n1 (inc n2)))))))))
