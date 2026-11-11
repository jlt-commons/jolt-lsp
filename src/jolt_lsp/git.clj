(ns jolt-lsp.git
  "The one git read the engine needs: a file's text at a revision."
  (:require [babashka.fs :as fs]
            [clojure.java.shell :as shell]))

(defn show
  "The text of `path` (absolute) at git revision `rev` in the repository
  containing `root`, \"\" when the file did not exist there, or nil when
  `root` is not in a git repository."
  [root rev path]
  (try
    (let [top (shell/sh "git" "-C" (str root) "rev-parse" "--show-toplevel")]
      (when (zero? (:exit top))
        (let [top (clojure.string/trim (:out top))
              rel (str (fs/relativize (fs/canonicalize top) (fs/path path)))
              r (shell/sh "git" "-C" top "show" (str rev ":" rel))]
          (if (zero? (:exit r)) (:out r) ""))))
    (catch Throwable _ nil)))
