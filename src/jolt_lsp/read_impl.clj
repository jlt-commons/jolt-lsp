(ns jolt-lsp.read-impl
  "The external reader boundary. rewrite-clj parses here; the rest of the
  engine works on nodes it returns.

  Parsing is tolerant: a broken tail keeps the forms before it, so an
  editor buffer mid-edit still indexes. Never throws."
  (:require [clojure.string :as string]
            [jolt-lsp.text :as text]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]))

(def ^:private ignored-tags #{:whitespace :newline :comment})

(defn- forms-of [root]
  (vec (remove #(ignored-tags (n/tag %)) (n/children root))))

(defn- error-candidates [s e]
  (let [d (ex-data e)
        msg (str e)
        [_ l c] (or (re-find #"\[at line (\d+), column (\d+)\]" msg)
                    (re-find #"\[line (\d+), col (\d+)\]" msg))
        row (or (:row d) (when l (read-string l)))
        col (or (:col d) (when c (read-string c)))
        at (when (and row col) (text/position->offset s [row col]))
        line-start (when row (text/position->offset s [row 1]))
        token (second (re-find #"(?:Invalid number|Invalid symbol|Invalid keyword|Invalid character): ([^\s.]+)" msg))
        token-at (when token (string/index-of s token))
        line-offs (map #(text/position->offset s [% 1])
                       (reverse (range 1 (inc (count (text/line-starts s))))))]
    (->> (concat [at token-at line-start] line-offs [0])
         (remove nil?)
         distinct)))

(defn- mask-tail [s offset]
  (str (subs s 0 offset)
       (apply str (map (fn [c] (if (= c \newline) \newline \space))
                       (subs s offset)))))

(defn- masked-parse [s offset]
  (try
    (let [root (p/parse-string-all (mask-tail s offset))
          forms (forms-of root)]
      {:ok false :root nil :forms forms :count (count forms) :error nil})
    (catch Throwable _ nil)))

(defn parse-text
  "Parse text into {:ok bool :root nil :forms vector :count n :error msg}.

  Forms before the first unreadable form are kept, with their real
  positions (the invalid tail is masked, not reparsed). :ok is false when
  a tail could not be read."
  [text-in]
  (let [s (str text-in)]
    (try
      (let [root (p/parse-string-all s)
            forms (forms-of root)]
        {:ok true :root nil :forms forms :count (count forms) :error nil})
      (catch Throwable e
        (let [msg (str e)]
          (or (some (fn [o] (when-let [r (masked-parse s o)] (assoc r :error msg)))
                    (error-candidates s e))
              {:ok false :root nil :forms [] :count 0 :error msg}))))))

(defn parse-edn
  "Parse an EDN file the same way, keeping every top-level form."
  [text]
  (parse-text text))
