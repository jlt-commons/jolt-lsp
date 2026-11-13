(ns jolt-lsp.paths
  "Project discovery and file enumeration. All host IO lives here."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [jolt.deps :as deps]
            [jolt.deps.ext :as deps-ext]))

(defn project-root
  "Walk up from `start` to the nearest directory holding deps.edn."
  [start]
  (loop [dir (fs/absolutize (str start))]
    (cond
      (fs/exists? (fs/path dir "deps.edn")) (str dir)
      (= (str dir) (str (fs/parent dir))) (str (fs/absolutize (str start)))
      :else (recur (fs/parent dir)))))

(defn read-deps [root]
  (try
    (edn/read-string (slurp (str (fs/path root "deps.edn"))))
    (catch Throwable _ {})))

(defn source-roots [root]
  (let [deps (read-deps root)
        paths (or (:paths deps) ["src" "test"])
        paths (concat paths
                      (mapcat (fn [[_ v]] (:extra-paths v)) (:aliases deps)))]
    (->> paths
         (filter string?)
         (map #(str (fs/path root %)))
         distinct
         vec)))

(defn- glob-all [root pattern]
  (try
    (mapv str (fs/glob root pattern))
    (catch Throwable _ [])))

(defn source-files
  "Every .clj, .cljc and .edn file under the project's source roots."
  [root]
  (let [roots (source-roots root)
        files (mapcat (fn [r]
                        (concat (glob-all r "*.clj")
                                (glob-all r "*.cljc")
                                (glob-all r "*.edn")
                                (glob-all r "**/*.clj")
                                (glob-all r "**/*.cljc")
                                (glob-all r "**/*.edn")))
                      roots)]
    (->> files
         (filter fs/regular-file?)
         (map fs/canonicalize)
         (map str)
         distinct
         sort
         vec)))

(defn dialect [path]
  (let [p (string/lower-case path)]
    (cond
      (string/ends-with? p ".cljc") :cljc
      (string/ends-with? p ".clj") :clj
      (string/ends-with? p ".edn") :edn
      :else :unknown)))

(defn file-uri [path]
  (str "file://" path))

(defn uri->path [uri]
  (when uri
    (-> uri
        (string/replace-first #"^file://" "")
        (string/replace "%20" " "))))

(defn relative [root path]
  (let [r (str root)]
    (if (string/starts-with? path r)
      (string/replace-first path r "")
      path)))

(defn subprojects
  "Directories under root (depth <= 4) that hold a deps.edn."
  [root]
  (->> (glob-all root "**/deps.edn")
       (map fs/parent)
       (map str)
       (filter #(not= % (str (fs/absolutize root))))
       distinct
       vec))

;; ---------------------------------------------------------------------------
;; dependencies

(defn- cache-dir []
  (or (System/getenv "JOLT_LSP_CACHE")
      (str (fs/home) "/.cache/jolt-lsp")))

(defn dep-roots
  "Dependency source roots and JARs, project roots removed."
  [root]
  (when-not (System/getenv "JOLT_LSP_NO_DEPS")
    (try
      (let [own (set (map #(str (fs/canonicalize %)) (source-roots root)))
            rs (:roots (deps/resolve-project root []))]
        (->> rs
             (map #(str (fs/absolutize (str %))))
             (remove (fn [p]
                       (try (contains? own (str (fs/canonicalize p)))
                            (catch Throwable _ false))))
             distinct
             vec))
      (catch Throwable _ []))))

(defn- extract-jar
  "Unpack a JAR into the cache once and return the directory. It unpacks into
  a temporary sibling and renames it into place, so an interrupted or failed
  unzip is never taken for a complete one on the next call."
  [jar]
  (let [dest (str (fs/path (cache-dir) "jars"
                           (str (hash (str jar (fs/size jar))))))]
    (when-not (fs/exists? dest)
      (let [tmp (str dest ".tmp-" (System/currentTimeMillis) "-" (rand-int 1000000))]
        (try
          (fs/create-dirs (fs/parent dest))
          (fs/unzip jar tmp)
          (when-not (fs/exists? dest) (fs/move tmp dest))
          (catch Throwable _ nil)
          (finally
            (when (fs/exists? tmp)
              (try (fs/delete-tree tmp) (catch Throwable _ nil)))))))
    (when (fs/exists? dest) dest)))

(defn- jar-file? [p]
  (and (fs/regular-file? p)
       (let [l (string/lower-case p)]
         (or (string/ends-with? l ".jar") (string/ends-with? l ".zip")))))

(defn dep-libraries
  "The libraries the project depends on, transitively, with the selected
  aliases' extra deps: each {:lib :version :kind :root :dir}. :dir is the
  directory holding the library's files, a git checkout, a local root or a
  JAR unpacked into the cache; libraries with no files on disk are left out."
  ([root] (dep-libraries root []))
  ([root aliases]
   (let [{:keys [libs]} (deps/resolve-project (str root) (mapv keyword aliases))]
     (->> libs
          (keep (fn [[lib coord]]
                  (let [info (try (deps-ext/coord-info lib coord) (catch Throwable _ nil))
                        r (some-> (:root info) str)
                        dir (cond
                              (nil? r) nil
                              (fs/directory? r) (str (fs/canonicalize r))
                              (jar-file? r) (some-> (extract-jar r) fs/canonicalize str))]
                    (when dir
                      {:lib (str lib)
                       :version (or (:mvn/version coord) (:git/tag coord)
                                    (:git/sha coord) (:local/root coord))
                       :kind (cond (:mvn/version coord) :mvn
                                   (:git/url coord) :git
                                   (:git/sha coord) :git
                                   (:local/root coord) :local
                                   :else :other)
                       :root r
                       :dir dir}))))
          (sort-by :lib)
          vec))))

(defn dep-files
  "Every .clj/.cljc source under dependency roots and extracted JARs."
  [root]
  (->> (dep-roots root)
       (mapcat (fn [r]
                 (cond
                   (fs/directory? r)
                   (concat (glob-all r "*.clj") (glob-all r "*.cljc")
                           (glob-all r "**/*.clj") (glob-all r "**/*.cljc"))
                   (and (fs/regular-file? r) (string/ends-with? r ".jar"))
                   (when-let [d (extract-jar r)]
                     (concat (glob-all d "**/*.clj") (glob-all d "**/*.cljc")))
                   :else [])))
       (filter fs/regular-file?)
       distinct
       sort
       vec))

