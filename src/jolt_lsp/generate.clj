(ns jolt-lsp.generate
  "Path-derived namespace names and tiny generated forms."
  (:require [babashka.fs :as fs]
            [clojure.string :as string]
            [jolt-lsp.paths :as paths]))

(defn ns-for-path
  "src/foo/bar_baz.clj -> foo.bar-baz, relative to the project root's source roots."
  [root path]
  (let [abs-path (str (fs/absolutize (str path)))
        no-root (->> (paths/source-roots root)
                     (map #(str (fs/absolutize (str %)) "/"))
                     (sort-by count >)
                     (some (fn [r]
                             (when (string/starts-with? abs-path r)
                               (subs abs-path (count r))))))
        rel (or no-root (string/replace abs-path (str (fs/absolutize (str root)) "/") ""))
        no-ext (string/replace rel #"\.(clj|cljc|cljs|edn)$" "")]
    (-> no-ext
        (string/replace #"/" ".")
        (string/replace #"_" "-"))))

(defn ns-form [ns-name]
  (str "(ns " ns-name ")\n"))
