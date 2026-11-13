(ns jolt-lsp.dep-source
  "Search and read the source files of a project's dependencies.

  Only the libraries the project's deps.edn resolves to (with any aliases
  asked for) are searched or read: git checkouts, local roots, and Maven
  JARs unpacked into the jolt-lsp cache. A read of a path outside those
  directories is refused.

  Searches use ripgrep when it is installed and a line-by-line regex scan
  otherwise.

  Adapted from clojure-mcp's deps_grep, deps_read and deps_list tools
  (https://github.com/bhauman/clojure-mcp, Copyright (c) 2025 Bruce Hauman,
  EPL-2.0). clojure-mcp reads entries out of JARs on the JVM classpath;
  here a dependency is a directory on disk."
  (:require [babashka.fs :as fs]
            [clojure.java.shell :as shell]
            [clojure.string :as string]
            [jolt-lsp.dep-text :as dt :refer [glob-matches? inside? library-matches?]]
            [jolt-lsp.paths :as paths]))

(def ^:private clip dt/clip)
(def ^:private pattern-re dt/pattern-re)
(def ^:private window dt/window)

;; ---------------------------------------------------------------------------
;; dependencies

(defn libraries
  "The project's dependency libraries ({:lib :version :kind :root :dir}),
  narrowed by a library filter when one is given."
  [root {:keys [aliases library]}]
  (filterv #(library-matches? library (:lib %))
           (paths/dep-libraries root (or aliases []))))

(defn- owner [libs file]
  (->> libs
       (filter #(inside? (:dir %) file))
       (sort-by #(- (count (:dir %))))
       first))

(defn- relative-to [dir file]
  (subs file (inc (count dir))))

;; ---------------------------------------------------------------------------
;; search

(def ^:dynamic *rg*
  "true to search with ripgrep, false for the built-in scan, nil to use
  ripgrep when it is installed."
  nil)

(defn rg-available? []
  (some? (fs/which "rg")))

(def ^:private binary-extensions
  #{".class" ".jar" ".war" ".ear" ".png" ".jpg" ".jpeg" ".gif" ".ico" ".bmp"
    ".webp" ".so" ".dll" ".dylib" ".o" ".a" ".zip" ".gz" ".tar" ".bz2" ".xz"
    ".ttf" ".otf" ".woff" ".woff2" ".pdf" ".doc" ".docx" ".wasm"})

(defn- binary-path? [p]
  (let [l (string/lower-case p)]
    (some #(string/ends-with? l %) binary-extensions)))

(def ^:private max-file-size (* 2 1024 1024))

(defn- hidden-segment? [rel]
  (some #(string/starts-with? % ".") (string/split rel #"/")))

(defn- scan-files [dir glob]
  (->> (try (fs/glob dir "**") (catch Throwable _ []))
       (map str)
       (filter fs/regular-file?)
       (remove binary-path?)
       (remove #(hidden-segment? (relative-to dir %)))
       (filter #(glob-matches? glob (relative-to dir %)))
       (filter #(<= (fs/size %) max-file-size))
       sort))

(defn- scan
  "The built-in search: [{:file :line :text}] over every text file under the
  dirs, sorted by path, stopping once `limit` rows are found."
  [dirs pattern {:keys [case-insensitive glob files-only]} limit]
  (let [re (pattern-re pattern case-insensitive)]
    (loop [files (mapcat #(scan-files % glob) dirs) out []]
      (if (or (empty? files) (>= (count out) limit))
        out
        (let [f (first files)
              text (try (slurp f) (catch Throwable _ nil))
              hits (when (and text (not (string/includes? text "\u0000")))
                     (if files-only
                       (when (some #(re-find re %) (string/split-lines text))
                         [{:file f}])
                       (keep-indexed (fn [i line]
                                       (when (re-find re line)
                                         {:file f :line (inc i) :text line}))
                                     (string/split-lines text))))]
          (recur (rest files) (into out (take (- limit (count out))) hits)))))))

(defn- parse-rg-line
  "One line of `rg --null --line-number` output: path NUL line:text."
  [line files-only]
  (if files-only
    (let [path (string/trim line)]
      (when-not (string/blank? path) {:file path}))
    (let [i (string/index-of line "\u0000")]
      (when i
        (let [path (subs line 0 i)
              more (subs line (inc i))
              j (string/index-of more ":")]
          (when j
            {:file path
             :line (parse-long (subs more 0 j))
             :text (subs more (inc j))}))))))

(defn- rg
  [dirs pattern {:keys [case-insensitive glob files-only]} limit]
  (let [cmd (cond-> ["rg" "--no-heading" "--with-filename" "--line-number" "--null"
                     "--color" "never" "--sort" "path" "--no-messages"
                     "--max-filesize" "2M" "--max-count" (str limit)]
              case-insensitive (conj "-i")
              glob (conj "-g" glob)
              files-only (conj "-l"))
        cmd (-> cmd (conj "-e" pattern "--") (into dirs))
        {:keys [exit out err]} (apply shell/sh cmd)]
    ;; 2 is also what rg exits with when it matched but could not read some
    ;; file (permissions, a broken symlink): keep what it found
    (cond
      (or (zero? exit) (and (= 2 exit) (not (string/blank? out))))
      (->> (if files-only (string/split out #"\u0000") (string/split-lines out))
           (keep #(parse-rg-line % files-only))
           (take limit)
           vec)
      (#{1 2} exit) []
      :else (throw (ex-info (str "rg: " (string/trim (str err))) {:exit exit})))))

(def ^:private default-limit 50)
(def ^:private max-limit 1000)

(defn grep
  "Search the dependencies' files for the regex `pattern`. Options:
  :library, :aliases, :glob, :case-insensitive, :files-only, :limit (rows,
  default 50). Returns {:results [{:lib :file :path :line :text}]}, with
  :truncated when the limit cut the search short."
  [root {:keys [pattern limit] :as opts}]
  (cond
    (string/blank? pattern) {:ok false :error "pass pattern"}
    :else
    (let [libs (libraries root opts)]
      (if (empty? libs)
        {:ok false
         :error (if (:library opts)
                  (str "no dependency matches " (:library opts) "; see jolt_deps_list")
                  "the project has no dependencies with sources on disk")}
        (let [limit (-> (or limit default-limit) (max 1) (min max-limit))
              use-rg (if (nil? *rg*) (rg-available?) *rg*)
              rows ((if use-rg rg scan) (mapv :dir libs) pattern opts (inc limit))
              rows (keep (fn [r]
                           (let [f (str (fs/canonicalize (:file r)))]
                             (when-let [lib (owner libs f)]
                               (cond-> (assoc r :lib (:lib lib) :file f
                                              :path (relative-to (:dir lib) f))
                                 (:text r) (update :text clip 300)))))
                         rows)]
          (cond-> {:results (vec (take limit rows))}
            (> (count rows) limit) (assoc :truncated true)))))))

;; ---------------------------------------------------------------------------
;; read

(defn- pick-library [libs wanted]
  (let [exact (filter #(= wanted (:lib %)) libs)]
    (cond
      (seq exact) (first exact)
      (= 1 (count libs)) (first libs))))

(defn read-file
  "Read a file inside one of the project's dependencies: :file (absolute, or
  relative to the project root), or :library plus :path relative to that
  library. :offset is the first line (1-based, default 1), :limit the number
  of lines. Paths outside the dependencies are refused."
  [root {:keys [file library path offset limit aliases]}]
  (let [libs (paths/dep-libraries root (or aliases []))
        target (cond
                 file (if (fs/absolute? file) file (str (fs/path root file)))
                 (and library path)
                 (let [cands (filterv #(library-matches? library (:lib %)) libs)]
                   (when-let [lib (pick-library cands library)]
                     (if (fs/absolute? path) path (str (:dir lib) "/" path)))))]
    (cond
      (and (nil? file) (not (and library path)))
      {:ok false :error "pass file, or library and path"}

      (nil? target)
      {:ok false :error (str "no single dependency matches " library "; see jolt_deps_list")}

      :else
      ;; containment is decided on the real path: the dirs are canonical, and
      ;; a symlink can lead into a dependency as well as out of one
      (let [real (str (fs/canonicalize target))
            lib (owner libs real)]
        (cond
          (nil? lib) {:ok false :error (str target " is not inside a dependency of this project")}
          (not (fs/regular-file? real)) {:ok false :error (str "no such file: " target)}
          :else (merge {:ok true :lib (:lib lib) :file real :path (relative-to (:dir lib) real)}
                       (window (slurp real) (or offset 1) limit)))))))
