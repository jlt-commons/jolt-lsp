(ns jolt-lsp.text.index
  "Cached position conversions for code that converts many positions in one
  text (a result list, an LSP response). jolt-lsp.text is the pure,
  spec-checked reference; this computes a text's line starts and the UTF-8
  byte offset of each line once, keeps the last few texts by identity, and
  answers each conversion from them."
  (:require [jolt-lsp.text :as text]))

(defn- utf8-length [cp]
  (cond (< cp 0x80) 1 (< cp 0x800) 2 (< cp 0x10000) 3 :else 4))

(defn- build [s]
  (let [starts (text/line-starts s)
        n (count s)
        ;; UTF-8 bytes before each line start
        byte-starts (first
                     (reduce (fn [[acc bytes prev] start]
                               (let [b (+ bytes (reduce + 0 (map #(utf8-length (int (nth s %)))
                                                                (range prev start))))]
                                 [(conj acc b) b start]))
                             [[] 0 0]
                             starts))]
    {:s s :n n :starts starts :byte-starts byte-starts}))

(def ^:private recent (atom []))

(defn index
  "The conversion index for s, cached by identity."
  [s]
  (or (some (fn [ix] (when (identical? (:s ix) s) ix)) @recent)
      (let [ix (build s)]
        (swap! recent #(vec (take 8 (cons ix %))))
        ix)))

(defn- clamp [x lo hi] (min hi (max lo x)))

(defn- line-end [{:keys [starts n]} row]
  (if (< row (count starts)) (nth starts row) n))

(defn position->offset
  "1-based [row col] -> clamped codepoint offset, as text/position->offset."
  [s [row col]]
  (let [{:keys [starts] :as ix} (index s)
        row (clamp row 1 (count starts))
        start (nth starts (dec row))
        end (line-end ix row)]
    (+ start (dec (clamp col 1 (inc (- end start)))))))

(defn- row-of [starts offset]
  ;; binary search: the last line start <= offset
  (loop [lo 0 hi (dec (count starts))]
    (if (>= lo hi)
      (inc lo)
      (let [mid (quot (+ lo hi 1) 2)]
        (if (<= (nth starts mid) offset) (recur mid hi) (recur lo (dec mid)))))))

(defn offset->position
  "Clamped codepoint offset -> 1-based [row col], as text/offset->position."
  [s offset]
  (let [{:keys [starts n]} (index s)
        o (clamp offset 0 n)
        row (row-of starts o)]
    [row (inc (- o (nth starts (dec row))))]))

(defn utf8-offset
  "Codepoint offset -> UTF-8 byte offset, as text/utf8-offset."
  [s cp]
  (let [{:keys [starts byte-starts n]} (index s)
        o (clamp cp 0 n)
        row (row-of starts o)
        start (nth starts (dec row))]
    (+ (nth byte-starts (dec row))
       (reduce + 0 (map #(utf8-length (int (nth s %))) (range start o))))))

(defn codepoint->utf16-col
  "0-based codepoint column on a 1-based row -> 0-based UTF-16 column. Lines
  are short, so this scans the line, with the line start cached."
  [s row col]
  (let [{:keys [starts] :as ix} (index s)
        row (clamp row 1 (count starts))
        start (nth starts (dec row))
        end (line-end ix row)
        content (- (if (and (> end start) (= \newline (nth s (dec end)))) (dec end) end) start)]
    (reduce (fn [u i] (+ u (if (> (int (nth s (+ start i))) 0xFFFF) 2 1)))
            0
            (range (clamp col 0 content)))))
