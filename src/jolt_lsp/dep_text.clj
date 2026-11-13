(ns jolt-lsp.dep-text
  "The pure parts of searching and reading dependency sources: glob and
  library filters, a line-by-line regex scan, a numbered line window, and
  the check that a path is inside a dependency directory.

  Adapted from clojure-mcp's deps_grep and deps_read tools
  (https://github.com/bhauman/clojure-mcp, Copyright (c) 2025 Bruce Hauman,
  EPL-2.0)."
  (:require [clojure.string :as string]))

(defn glob-matches?
  "Does `path` end in something matching `glob`? `*` matches any run of
  characters, `?` one, and `{a,b}` either alternative. No glob matches
  everything."
  [glob path]
  (if-not glob
    true
    (let [re (-> glob
                 (string/replace #"[.+^$|()\[\]\\]" "\\\\$0")
                 (string/replace "*" ".*")
                 (string/replace "?" ".")
                 (string/replace #"\{([^}]+)\}"
                                 (fn [[_ alts]]
                                   (str "(" (string/replace alts "," "|") ")"))))]
      (boolean (re-find (re-pattern (str re "$")) path)))))

(defn library-matches?
  "Does the lib name match `filter`? A filter is a full lib (group/artifact),
  a group, or an artifact name. No filter matches every lib."
  [wanted lib]
  (if (string/blank? wanted)
    true
    (let [[group artifact] (if (string/includes? lib "/")
                             (string/split lib #"/" 2)
                             [lib lib])]
      (or (= wanted lib) (= wanted group) (= wanted artifact)))))

(defn pattern-re [pattern case-insensitive]
  (re-pattern (if case-insensitive (str "(?i)" pattern) pattern)))

(defn grep-lines
  "The lines of `text` the regex `pattern` finds something in, each
  {:line n :text s} with n 1-based."
  [text pattern case-insensitive]
  (let [re (pattern-re pattern case-insensitive)]
    (into []
          (keep-indexed (fn [i line]
                          (when (re-find re line)
                            {:line (inc i) :text line})))
          (string/split-lines text))))

(def ^:private max-line-length 2000)

(defn clip [s n]
  (if (> (count s) n) (str (subs s 0 n) "...") s))

(defn window
  "Lines `start` (1-based) to start+limit-1 of `text`, numbered. With no
  limit, to the end of the file."
  [text start limit]
  (let [lines (vec (string/split-lines text))
        total (count lines)
        start (max 1 start)
        stop (if limit (min total (+ start (max 0 limit) -1)) total)
        shown (if (<= start stop) (subvec lines (dec start) stop) [])]
    {:content (string/join "\n" (map-indexed (fn [i l]
                                               (format "%6d\t%s" (+ start i) (clip l max-line-length)))
                                             shown))
     :start start
     :end (min total (max (dec start) stop))
     :total-lines total
     :truncated (< stop total)}))

(defn- normalize [p]
  (let [out (reduce (fn [out x]
                      (cond
                        (or (= x "") (= x ".")) out
                        (= x "..") (if (seq out) (pop out) out)
                        :else (conj out x)))
                    []
                    (string/split p #"/"))]
    (str (when (string/starts-with? p "/") "/") (string/join "/" out))))

(defn inside?
  "Is `path` strictly below the directory `dir`, after `.` and `..` are
  resolved?"
  [dir path]
  (let [d (normalize dir)
        p (normalize path)]
    (and (not= d p)
         (string/starts-with? p (if (string/ends-with? d "/") d (str d "/"))))))
