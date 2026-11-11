(ns jolt-lsp.read-impl
  "The reader boundary. jolt-lsp.parse parses here; the rest of the
  engine works on nodes it returns.

  Parsing is tolerant: a broken top-level form is masked out (replaced by
  blanks, so every other position is unchanged) and the forms around it
  still index, so an editor buffer or an agent's half-written file keeps
  working. The delimiter scan in jolt-lsp.syntax finds the broken form, so a
  broken file costs a handful of parses, not one per line. Never throws."
  (:require [clojure.string :as string]
            [jolt-lsp.parse :as p]
            [jolt-lsp.syntax :as syntax]
            [jolt-lsp.text :as text]))

(def ^:private ignored-tags #{:whitespace :newline :comment :comma})

(defn- forms-of [root]
  (vec (remove #(ignored-tags (:tag %)) (:all root))))

(def ^:dynamic *skim-depth*
  "Collections nested this deep are skipped, not parsed (nil: parse all)."
  nil)

(defn- try-parse [s]
  (try
    {:forms (forms-of (p/parse-string-all s *skim-depth*))}
    (catch Throwable e {:exception e})))

(defn- token-index
  "The first place `token` stands as a whole token in s (not inside a
  longer one, as `a/` sits inside `:a/x`), else its first occurrence."
  [s token]
  (let [delim? (fn [i] (or (neg? i) (>= i (count s))
                           (let [c (nth s i)]
                             (or (Character/isWhitespace c) (#{\( \) \[ \] \{ \} \, \"} c)))))
        hits (take-while some? (iterate #(when % (string/index-of s token (inc %)))
                                        (string/index-of s token)))]
    (or (first (filter #(and (delim? (dec %)) (delim? (+ % (count token)))) (take 1000 hits)))
        (first hits))))

(defn- reader-error-offset
  "The offset the reader blamed, from its ex-data or its message."
  [s e]
  (let [d (ex-data e)
        msg (str e)
        [_ l c] (or (re-find #"\[at line (\d+), column (\d+)\]" msg)
                    (re-find #"\[line (\d+), col (\d+)\]" msg))
        row (or (:row d) (:line d) (when l (parse-long l)))
        col (or (:col d) (when c (parse-long c)))
        ;; "Invalid number: 1.2.3." carries no position: find the token
        token (second (re-find #"(?:Invalid \w+|Unsupported character): (\S+?)\.?$"
                               (str (ex-message e))))]
    (cond
      (and row col) (text/position->offset s [row col])
      token (token-index s token))))

(defn- margin-starts
  "Offsets of lines that start a top-level form: something other than
  whitespace, a comment, or a closer in column 1. A heuristic (a string can
  hold such a line), good enough to bound a broken form."
  [s]
  (let [n (count s)]
    (loop [i 0 acc []]
      (if (>= i n)
        acc
        (let [nl (string/index-of s "\n" i)
              c (nth s i)
              acc (if (or (Character/isWhitespace c) (#{\; \) \] \}} c))
                    acc
                    (conj acc i))]
          (if nl (recur (inc nl) acc) acc))))))

(defn- string-end [s n i]
  (reduce (fn [esc j]
            (cond
              esc false
              (= (nth s j) \\) true
              (= (nth s j) \") (reduced (inc j))
              :else false))
          false
          (range (inc i) n)))

(defn- top-starts
  "Offsets where an element starts at depth 0, by a lexer that skips
  strings, comments and char literals. A closer with nothing open is its
  own element."
  [s]
  (let [n (count s)
        step (fn [[i depth sep acc]]
               (let [c (nth s i)
                     start? (and (zero? depth) sep)
                     acc+ (if start? (conj acc i) acc)]
                 (cond
                   (= c \\) [(+ i 2) depth false acc+]
                   (= c \") (let [e (string-end s n i)]
                              [(if (number? e) e n) depth false acc+])
                   (= c \;) [(if-let [nl (string/index-of s "\n" i)] (inc nl) n) depth true acc]
                   (or (Character/isWhitespace c) (= c \,)) [(inc i) depth true acc]
                   (#{\( \[ \{} c) [(inc i) (inc depth) false acc+]
                   (#{\) \] \}} c) [(inc i) (max 0 (dec depth)) true
                                    (if (zero? depth) (conj acc i) acc)]
                   :else [(inc i) depth false acc+])))]
    (peek (reduce (fn [st _] (if (< (first st) n) (step st) (reduced st)))
                  [0 0 true []]
                  (range (inc n))))))

(defn- region-around
  "[start end) of the top-level element holding offset `at`: from the last
  depth-0 start at or before it to the next depth-0 or margin start after
  it (a margin start bounds a form left unclosed)."
  [starts margins n at]
  (let [start (or (last (filter #(<= % at) starts)) 0)
        end (or (first (sort (filter #(> % at) (concat starts margins)))) n)]
    [start end]))

(defn- mask-tail [s start]
  (str (subs s 0 start)
       (apply str (map (fn [c] (if (= c \newline) \newline \space)) (subs s start)))))

(defn- first-unreadable
  "The depth-0 start of the first element that does not read, by binary
  search over the prefixes that do: for reader errors with no position."
  [s starts]
  (let [ok? (fn [k] (or (zero? k) (contains? (try-parse (mask-tail s (nth starts k))) :forms)))
        ;; largest k whose prefix (elements before starts[k]) reads
        k (reduce (fn [[lo hi] _]
                    (if (>= lo hi)
                      (reduced [lo hi])
                      (let [mid (quot (+ lo hi 1) 2)]
                        (if (ok? mid) [mid hi] [lo (dec mid)]))))
                  [0 (dec (count starts))]
                  (range 64))]
    (when (seq starts) (nth starts (first k)))))

(defn- bad-offset
  "Where the text first goes wrong: the outermost unclosed opener, the stray
  or mismatched closer, the unterminated string, or the reader's own blame."
  [s e]
  (let [{:keys [balance stack at]} (syntax/scan s)]
    (case balance
      :unclosed (second (first stack))
      (:stray :mismatch :unterminated-string) at
      (or (reader-error-offset s e)
          (first-unreadable s (top-starts s))))))

(defn- mask [s [start end]]
  (str (subs s 0 start)
       (apply str (map (fn [c] (if (= c \newline) \newline \space))
                       (subs s start end)))
       (subs s end)))

(def ^:private max-masks 16)

(defn parse-text
  "Parse text into {:ok bool :root nil :forms vector :count n :error msg
  :broken [[start end] ...]}.

  :ok is false when some of the text could not be read. :forms are the forms
  that could, at their real positions; :broken holds the masked codepoint
  ranges and :error the first reader complaint."
  [text-in]
  (let [s (str text-in)
        r (try-parse s)]
    (if-let [forms (:forms r)]
      {:ok true :root nil :forms forms :count (count forms) :error nil :broken []}
      (let [msg (str (:exception r))
            margins (margin-starts s)
            n (count s)]
        (loop [cur s, e (:exception r), broken [], i 0]
          (let [at (bad-offset cur e)
                ;; masked text starts no element
                starts (top-starts cur)
                region (when (and at (< i max-masks)) (region-around starts margins n at))]
            (if (or (nil? region) (some #{region} broken))
              ;; no progress: drop everything from the blamed form on
              (let [start (if at (first (region-around starts margins n at)) 0)
                    tail [start n]
                    r2 (try-parse (mask cur tail))
                    forms (or (:forms r2) [])]
                {:ok false :root nil :forms forms :count (count forms)
                 :error msg :broken (conj broken tail)})
              (let [masked (mask cur region)
                    r2 (try-parse masked)
                    broken (conj broken region)]
                (if-let [forms (:forms r2)]
                  {:ok false :root nil :forms forms :count (count forms)
                   :error msg :broken broken}
                  (recur masked (:exception r2) broken (inc i)))))))))))

(defn parse-edn
  "Parse an EDN file the same way, keeping every top-level form."
  [text]
  (parse-text text))
