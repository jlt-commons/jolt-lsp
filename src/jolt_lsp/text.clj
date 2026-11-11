(ns jolt-lsp.text
  "Text geometry.

  Internal positions are 1-based and count codepoints: [row col]. `end-col`
  is exclusive. LSP positions are 0-based and count UTF-16 code units. Byte
  offsets are UTF-8. All conversions live here."
  (:require [clojure.string]))

(defn- codepoint-at [s i]
  (int (nth s i)))

(defn- utf16-units [cp]
  (if (> cp 65535) 2 1))

(defn- utf8-length [cp]
  (cond
    (< cp 128) 1
    (< cp 2048) 2
    (< cp 65536) 3
    :else 4))

(defn- line-end
  [s starts row]
  (if (< row (count starts))
    (nth starts row)
    (count s)))

(defn- content-length
  [s start end]
  (let [e (if (and (> end start) (= \newline (nth s (dec end)))) (dec end) end)
        e (if (and (> e start) (= \return (nth s (dec e)))) (dec e) e)]
    (- e start)))

(defn- clamp [n lo hi]
  (min hi (max lo n)))

(defn line-starts
  "Codepoint offsets of every line start. The first is always 0."
  [s]
  (reduce (fn [acc _]
            (if-let [i (clojure.string/index-of s "\n" (peek acc))]
              (conj acc (inc i))
              (reduced acc)))
          [0]
          (range (inc (count s)))))

(defn offset->position
  "Clamped codepoint offset -> 1-based [row col]."
  [s offset]
  (let [starts (line-starts s)
        o (clamp offset 0 (count s))
        row (count (take-while (fn [start] (<= start o)) starts))
        start (nth starts (dec row))]
    [row (inc (- o start))]))

(defn position->offset
  "1-based [row col] -> clamped codepoint offset."
  [s position]
  (let [row (first position)
        col (second position)
        starts (line-starts s)
        row (clamp row 1 (count starts))
        start (nth starts (dec row))
        end (line-end s starts row)
        col (clamp col 1 (inc (- end start)))]
    (+ start (dec col))))

(defn line-utf16-length
  "UTF-16 code units in a line's content, excluding the newline."
  [s row]
  (let [starts (line-starts s)
        row (clamp row 1 (count starts))
        start (nth starts (dec row))
        end (line-end s starts row)
        n (content-length s start end)]
    (reduce (fn [units i]
              (+ units (utf16-units (codepoint-at s (+ start i)))))
            0
            (range n))))

(defn utf16->codepoint-col
  "0-based UTF-16 column on a 1-based row -> 0-based codepoint column."
  [s row col]
  (let [starts (line-starts s)
        row (clamp row 1 (count starts))
        start (nth starts (dec row))
        end (line-end s starts row)
        n (content-length s start end)]
    (second
     (reduce (fn [state i]
               (let [units (first state)
                     cps (second state)
                     u (utf16-units (codepoint-at s (+ start i)))]
                 (if (< units col)
                   [(+ units u) (inc cps)]
                   [units cps])))
             [0 0]
             (range n)))))

(defn codepoint->utf16-col
  "0-based codepoint column on a 1-based row -> 0-based UTF-16 column."
  [s row col]
  (let [starts (line-starts s)
        row (clamp row 1 (count starts))
        start (nth starts (dec row))
        end (line-end s starts row)
        n (content-length s start end)]
    (reduce (fn [units i]
              (+ units (utf16-units (codepoint-at s (+ start i)))))
            0
            (range (clamp col 0 n)))))

(defn utf8-offset
  "Codepoint offset -> UTF-8 byte offset."
  [s cp]
  (reduce (fn [bytes i]
            (+ bytes (utf8-length (codepoint-at s i))))
          0
          (range (clamp cp 0 (count s)))))
