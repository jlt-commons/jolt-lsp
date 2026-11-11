(ns jolt-lsp.main
  "CLI entrypoint: agent commands, plus lsp and mcp modes."
  (:require [jolt-lsp.cli :as cli]
            [jolt-lsp.lsp :as lsp]
            [jolt-lsp.mcp :as mcp]
            [jolt-lsp.version :as version]))

(defn -main [& args]
  (case (first args)
    ("version" "--version" "-V") (println version/version)
    "mcp" (mcp/-main)
    "lsp" (lsp/-main)
    (apply cli/-main args)))
