(ns jolt-lsp.parse
  "A fast, position-tracking Clojure parser for the index.

  Produces rewrite-clj's tags and child layout, except that whitespace,
  newlines and commas are not nodes (comments are): the text between two
  nodes is in the source, by offset. One pass, and a node's text is cut from
  the source only when asked for.

  It rejects what the reader rejects (unbalanced or mismatched delimiters,
  unterminated strings, a prefix with no form, malformed numbers, symbols
  and keywords), throwing ex-info with the 1-based :row and :col of the
  problem, so the tolerant reader can mask the broken form."
  (:require [clojure.string :as string]))

(defrecord Node [tag kind src start end row col end-row end-col all kids])

(defn node-text [^Node n]
  (subs (:src n) (:start n) (:end n)))

(def ^:private ws-tags #{:whitespace :newline :comma :comment})

(defn- mk [tag kind src start end row col end-row end-col all]
  (->Node tag kind src start end row col end-row end-col all
          (when all (if (some #(identical? :comment (:tag %)) all)
                      (filterv #(not (ws-tags (:tag %))) all)
                      all))))

;; ---------------------------------------------------------------------------
;; the cursor: index, row and column, advanced together

(deftype Cursor [s n st])     ; st: long-array [index row col depth skim-depth]

(defn- cur [s skim] (Cursor. s (count s) (long-array [0 1 1 0 (or skim 0)])))

(defn- ci [^Cursor c] (aget ^longs (.-st c) 0))
(defn- crow [^Cursor c] (aget ^longs (.-st c) 1))
(defn- ccol [^Cursor c] (aget ^longs (.-st c) 2))

(defn- peek-at [^Cursor c k]
  (let [j (+ (ci c) k)]
    (when (< j (.-n c)) (nth (.-s c) j))))

(defn- advance! [^Cursor c]
  (let [st (.-st c)
        i (aget st 0)
        ch (nth (.-s c) i)]
    (aset st 0 (inc i))
    (if (= ch \newline)
      (do (aset st 1 (inc (aget st 1))) (aset st 2 1))
      (aset st 2 (inc (aget st 2))))
    ch))

(defn- fail [^Cursor c msg]
  (throw (ex-info (str msg " [at line " (crow c) ", column " (ccol c) "]")
                  {:row (crow c) :col (ccol c) :msg msg})))

;; ---------------------------------------------------------------------------
;; characters

(defn- whitespace? [ch]
  (and ch (case ch
            (\space \tab \newline \return \formfeed) true
            (if (< (int ch) 128) false (Character/isWhitespace ch)))))

(defn- token-end? [ch]
  (or (nil? ch)
      (case ch
        (\space \tab \newline \return \formfeed \( \) \[ \] \{ \} \" \; \,) true
        (if (< (int ch) 128) false (Character/isWhitespace ch)))))

(defn- skip!
  "Advance the cursor to index j on the same line."
  [^Cursor c j]
  (let [st (.-st c)]
    (aset st 2 (+ (aget st 2) (- j (aget st 0))))
    (aset st 0 j)))

(defn- scan-token-end [^String s n i]
  (loop [j i]
    (if (and (< j n) (not (token-end? (nth s j)))) (recur (inc j)) j)))

(def ^:private closer-of {\( \) \[ \] \{ \}})

;; ---------------------------------------------------------------------------
;; tokens

(def ^:private number-re
  #"[+-]?(?:\d+(?:\.\d*)?(?:[eE][+-]?\d+)?M?|\d+N|0[xX][0-9a-fA-F]+N?|\d+[rR][0-9a-zA-Z]+|\d+/\d+)")

(def ^:private symbol-re
  ;; ns/name, name, the bare "/", or ns//
  #"(?:[^\s\d:#'`~@^\\/;\"(){}\[\],][^\s;\"(){}\[\],]*?(?:/(?:[^\s\d;\"(){}\[\],/][^\s;\"(){}\[\],]*|/))?|/)")

(def ^:private keyword-re
  #"::?[^\s:;\"(){}\[\],/][^\s;\"(){}\[\],]*?(?:/[^\s;\"(){}\[\],/][^\s;\"(){}\[\],]*)?")

(defn- plain-symbol?
  "The common case, settled without a regex: starts with a letter or one of
  *+!-_?<>=&%$|. and holds no colon pair or trailing slash."
  [t]
  (let [c0 (nth t 0)
        n (count t)]
    (and (or (Character/isLetter c0)
             (case c0 (\* \! \_ \? \< \> \= \& \% \$ \| \.) true false)
             (and (case c0 (\+ \-) true false)
                  (or (= n 1) (not (Character/isDigit (nth t 1))))))
         (not (string/includes? t "::"))
         (or (= n 1) (not= \/ (nth t (dec n)))))))

(defn- classify [t]
  (let [c0 (first t)
        c1 (second t)]
    (cond
      (and (not= c0 \:) (plain-symbol? t)
           (not (#{"nil" "true" "false"} t)))
      :symbol
      (= c0 \:) (when (re-matches keyword-re t) :keyword)
      (or (Character/isDigit c0)
          (and (#{\+ \-} c0) c1 (Character/isDigit c1)))
      (when (re-matches number-re t) :number)
      (#{"nil" "true" "false"} t) :literal
      (re-matches symbol-re t) :symbol
      :else nil)))

(defn- read-char-literal [^Cursor c]
  ;; \x, \newline, A, \o12 -- a backslash takes at least one character
  (advance! c)
  (when (>= (ci c) (.-n c)) (fail c "EOF after \\"))
  (advance! c)
  (loop []
    (when-not (token-end? (peek-at c 0))
      (advance! c)
      (recur))))

;; ---------------------------------------------------------------------------
;; the parser

(declare read-form read-coll*)

(defn- read-ws
  "A whitespace, newline, comma or comment node at the cursor, or nil."
  [^Cursor c]
  (let [s (.-s c)
        start (ci c) row (crow c) col (ccol c)
        ch (peek-at c 0)]
    (cond
      (nil? ch) nil
      (or (whitespace? ch) (= ch \,))
      (let [n (.-n c)
            st (.-st c)]
        ;; one run of blanks, newlines and commas, tracked with locals
        (loop [j start r (aget st 1) cl (aget st 2)]
          (let [x (when (< j n) (nth s j))]
            (cond
              (= x \newline) (recur (inc j) (inc r) 1)
              (or (= x \,) (whitespace? x)) (recur (inc j) r (inc cl))
              :else (do (aset st 0 j) (aset st 1 r) (aset st 2 cl)))))
        ::blank)
      (or (= ch \;) (and (= ch \#) (= \! (peek-at c 1))))
      (do (skip! c (or (string/index-of s "\n" start) (.-n c)))
          ;; a comment owns its newline, as in rewrite-clj
          (when (= \newline (peek-at c 0)) (advance! c))
          (mk :comment nil s start (ci c) row col (crow c) (ccol c) nil))
      :else nil)))

(defn- read-until-form
  "Whitespace nodes, then the next form; [ws-nodes form]. Fails at a closer
  or EOF: `what` needs a form."
  [^Cursor c what]
  (loop [acc []]
    (if-let [w (read-ws c)]
      (recur (if (identical? w ::blank) acc (conj acc w)))
      (let [ch (peek-at c 0)]
        (when (or (nil? ch) (#{\) \] \}} ch))
          (fail c (str what " needs a form after it")))
        [acc (read-form c)]))))

(defn- skim-coll
  "Skip a collection whose opener is at the cursor, without building nodes:
  a delimiter count that steps over strings, chars and comments. Leaves the
  cursor after the closer."
  [^Cursor c]
  (let [^String s (.-s c) n (.-n c) st (.-st c)
        row0 (aget st 1)]
    (loop [j (inc (aget st 0)) depth 1 r row0 cl (inc (aget st 2))]
      (if (>= j n)
        (do (aset st 0 j) (aset st 1 r) (aset st 2 cl)
            (fail c (str "EOF while reading, starting at line " row0)))
        (let [ch (nth s j)]
          (case ch
            (\( \[ \{) (recur (inc j) (inc depth) r (inc cl))
            (\) \] \}) (if (= depth 1)
                         (do (aset st 0 (inc j)) (aset st 1 r) (aset st 2 (inc cl)))
                         (recur (inc j) (dec depth) r (inc cl)))
            \newline (recur (inc j) depth (inc r) 1)
            \\ (recur (+ j 2) depth r (+ cl 2))
            \; (let [e (or (string/index-of s "\n" j) n)]
                 (recur e depth r (+ cl (- e j))))
            \" (let [[e r cl] (loop [k (inc j) r r cl (inc cl)]
                                (cond
                                  (>= k n) [k r cl]
                                  (= (nth s k) \\) (recur (+ k 2) r (+ cl 2))
                                  (= (nth s k) \newline) (recur (inc k) (inc r) 1)
                                  (= (nth s k) \") [(inc k) r (inc cl)]
                                  :else (recur (inc k) r (inc cl))))]
                 (recur e depth r cl))
            (recur (inc j) depth r (inc cl))))))))

(defn- read-coll [^Cursor c tag open-len]
  (let [s (.-s c)
        st (.-st c)
        start (ci c) row (crow c) col (ccol c)
        opener (nth s (+ start (dec open-len)))
        closer (closer-of opener)
        skim (aget st 4)]
    (if (and (pos? skim) (>= (aget st 3) skim))
      (do (dotimes [_ (dec open-len)] (advance! c))
          (skim-coll c)
          (mk tag nil s start (ci c) row col (crow c) (ccol c) []))
      (do (aset st 3 (inc (aget st 3)))
          (let [node (read-coll* c tag open-len s start row col closer)]
            (aset st 3 (dec (aget st 3)))
            node)))))

(defn- read-coll* [^Cursor c tag open-len s start row col closer]
  (do
    (dotimes [_ open-len] (advance! c))
    (loop [acc []]
      (if-let [w (read-ws c)]
        (recur (if (identical? w ::blank) acc (conj acc w)))
        (let [ch (peek-at c 0)]
          (cond
            (nil? ch) (fail c (str "EOF while reading, starting at line " row))
            (= ch closer) (do (advance! c)
                              (mk tag nil s start (ci c) row col (crow c) (ccol c) acc))
            (#{\) \] \}} ch) (fail c (str "Unmatched delimiter: " ch ", expected: " closer))
            :else (recur (conj acc (read-form c)))))))))

(defn- read-string-node [^Cursor c tag start row col]
  (let [s (.-s c)
        n (.-n c)
        st (.-st c)]
    (advance! c)                        ; the opening quote
    (loop [j (aget st 0) r (aget st 1) cl (aget st 2)]
      (if (>= j n)
        (do (aset st 0 j) (aset st 1 r) (aset st 2 cl)
            (fail c (str "EOF while reading string, starting at line " row)))
        (let [ch (nth s j)]
          (cond
            (= ch \\) (if (and (< (inc j) n) (= \newline (nth s (inc j))))
                         (recur (+ j 2) (inc r) 1)
                         (recur (+ j 2) r (+ cl 2)))
            (= ch \newline) (recur (inc j) (inc r) 1)
            (= ch \") (do (aset st 0 (inc j)) (aset st 1 r) (aset st 2 (inc cl))
                          (mk tag :string s start (inc j) row col r (inc cl) nil))
            :else (recur (inc j) r (inc cl))))))))

(defn- read-wrapped
  "A prefix of `n` characters and the form it applies to."
  [^Cursor c tag n what]
  (let [s (.-s c) start (ci c) row (crow c) col (ccol c)]
    (dotimes [_ n] (advance! c))
    (let [[ws form] (read-until-form c what)]
      (mk tag nil s start (ci c) row col (crow c) (ccol c) (conj ws form)))))

(defn- read-token [^Cursor c]
  (let [s (.-s c) start (ci c) row (crow c) col (ccol c)]
    (skip! c (scan-token-end s (.-n c) start))
    (let [t (subs s start (ci c))
          kind (classify t)]
      (when-not kind
        (throw (ex-info (str "Invalid token: " t " [at line " row ", column " col "]")
                        {:row row :col col :msg (str "Invalid token: " t)})))
      (mk (if (= kind :keyword) :keyword :token) kind s start (ci c) row col
          (crow c) (ccol c) nil))))

(defn- read-dispatch
  "Everything after #."
  [^Cursor c]
  (let [s (.-s c) start (ci c) row (crow c) col (ccol c)
        nxt (peek-at c 1)]
    (case nxt
      \( (read-coll c :fn 2)
      \{ (read-coll c :set 2)
      \" (do (advance! c) (read-string-node c :regex start row col))
      \' (read-wrapped c :var 2 "#'")
      \_ (read-wrapped c :uneval 2 "#_")
      \^ (let [_ (dotimes [_ 2] (advance! c))
               [ws1 m] (read-until-form c "#^")
               [ws2 f] (read-until-form c "metadata")]
           (mk :meta nil s start (ci c) row col (crow c) (ccol c)
               (-> ws1 (conj m) (into ws2) (conj f))))
      \= (read-wrapped c :eval 2 "#=")
      \# (do (dotimes [_ 2] (advance! c))
             (read-token c)
             (mk :token :literal s start (ci c) row col (crow c) (ccol c) nil))
      \: (let [_ (advance! c)
               qstart (ci c) qrow (crow c) qcol (ccol c)]
           ;; #:ns{...} or #::{...} / #::alias{...}
           (advance! c)
           (loop [] (let [x (peek-at c 0)]
                      (when (and x (not= x \{) (not (token-end? x))) (advance! c) (recur))))
           (let [q (mk :map-qualifier nil s qstart (ci c) qrow qcol (crow c) (ccol c) nil)]
             (when-not (= \{ (peek-at c 0)) (fail c "Namespaced map must be followed by a map"))
             (let [m (read-coll c :map 1)]
               (mk :namespaced-map nil s start (ci c) row col (crow c) (ccol c) [q m]))))
      \? (let [_ (dotimes [_ 2] (advance! c))
               splice? (= \@ (peek-at c 0))
               _ (when splice? (advance! c))
               t (mk :token :symbol s (inc start) (ci c) row (inc col) (crow c) (ccol c) nil)]
           (when-not (= \( (peek-at c 0)) (fail c "Reader conditional must be a list"))
           (let [body (read-coll c :list 1)]
             (mk :reader-macro nil s start (ci c) row col (crow c) (ccol c) [t body])))
      (nil \space \tab \newline \) \] \}) (do (advance! c) (fail c "EOF or space after #"))
      ;; a tagged literal: #inst "...", #my/tag {...}
      (let [_ (advance! c)
            tag-node (read-token c)
            [ws form] (read-until-form c (str "#" (node-text tag-node)))]
        (mk :reader-macro nil s start (ci c) row col (crow c) (ccol c)
            (-> [tag-node] (into ws) (conj form)))))))

(defn- read-form [^Cursor c]
  (let [s (.-s c) start (ci c) row (crow c) col (ccol c)
        ch (peek-at c 0)]
    (case ch
      \( (read-coll c :list 1)
      \[ (read-coll c :vector 1)
      \{ (read-coll c :map 1)
      (\) \] \}) (fail c (str "Unmatched delimiter: " ch))
      \" (read-string-node c :string start row col)
      \' (read-wrapped c :quote 1 "'")
      \` (read-wrapped c :syntax-quote 1 "`")
      \~ (if (= \@ (peek-at c 1))
           (read-wrapped c :unquote-splicing 2 "~@")
           (read-wrapped c :unquote 1 "~"))
      \@ (read-wrapped c :deref 1 "@")
      \^ (let [_ (advance! c)
               [ws1 m] (read-until-form c "^")
               [ws2 f] (read-until-form c "metadata")]
           (mk :meta nil s start (ci c) row col (crow c) (ccol c)
               (-> ws1 (conj m) (into ws2) (conj f))))
      \\ (do (read-char-literal c)
             (mk :token :char s start (ci c) row col (crow c) (ccol c) nil))
      \# (read-dispatch c)
      (read-token c))))

(defn parse-string-all
  "Parse every form in s into a :forms node. Throws ex-info with :row and
  :col on text the reader would reject. With `skim-depth`, collections
  nested that deep are skipped, not parsed: they come back with no
  children (for files only their definitions matter in)."
  [s & [skim-depth]]
  (let [s (str s)
        c (cur s skim-depth)]
    (loop [acc []]
      (if (>= (ci c) (.-n c))
        (mk :forms nil s 0 (.-n c) 1 1 (crow c) (ccol c) acc)
        (if-let [w (read-ws c)]
          (recur (if (identical? w ::blank) acc (conj acc w)))
          (recur (conj acc (read-form c))))))))
