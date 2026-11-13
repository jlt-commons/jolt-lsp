(ns jolt-lsp.cli
  "JSON/EDN CLI over the engine. One-shot, or `lsp`/`mcp` for a server."
  (:require [clojure.pprint :as pp]
            [clojure.string :as string]
            [jolt-lsp.api :as api]
            [jolt-lsp.json :as json]))

(defn- parse-args [args]
  (loop [args args
         opts {:json false :apply false :all false}
         pos []]
    (if (empty? args)
      (assoc opts :cmd (first pos) :pos (vec (rest pos)))
      (let [a (first args)]
        (case a
          "--json" (recur (rest args) (assoc opts :json true) pos)
          "--apply" (recur (rest args) (assoc opts :apply true) pos)
          "--fix" (recur (rest args) (assoc opts :fix true) pos)
          "--every" (recur (rest args) (assoc opts :every true) pos)
          "--refresh" (recur (rest args) (assoc opts :refresh true) pos)
          "--code" (recur (rest (rest args)) (assoc opts :code (second args)) pos)
          "--before" (recur (rest (rest args)) (assoc opts :before (second args)) pos)
          "--occurrence" (recur (rest (rest args)) (assoc opts :occurrence (parse-long (second args))) pos)
          "--all" (recur (rest args) (assoc opts :all true) pos)
          "--kondo" (recur (rest args) (assoc opts :kondo true) pos)
          "--no-kondo" (recur (rest args) (assoc opts :kondo false) pos)
          "--root" (recur (rest (rest args)) (assoc opts :root (second args)) pos)
          "--limit" (recur (rest (rest args)) (assoc opts :limit (parse-long (second args))) pos)
          "--direction" (recur (rest (rest args)) (assoc opts :direction (keyword (second args))) pos)
          "--alias" (recur (rest (rest args)) (assoc opts :alias (second args)) pos)
          "--old" (recur (rest (rest args)) (assoc opts :old-form (second args)) pos)
          "--new" (recur (rest (rest args)) (assoc opts :new-text (second args)) pos)
          "--after" (recur (rest (rest args)) (assoc opts :after (second args)) pos)
          "--rev" (recur (rest (rest args)) (assoc opts :rev (second args)) pos)
          "--to-ns" (recur (rest (rest args)) (assoc opts :to-ns (second args)) pos)
          "--library" (recur (rest (rest args)) (assoc opts :library (second args)) pos)
          "--glob" (recur (rest (rest args)) (assoc opts :glob (second args)) pos)
          "--aliases" (recur (rest (rest args)) (assoc opts :aliases (second args)) pos)
          "--offset" (recur (rest (rest args)) (assoc opts :offset (parse-long (second args))) pos)
          "--ignore-case" (recur (rest args) (assoc opts :case-insensitive true) pos)
          "--files-only" (recur (rest args) (assoc opts :files-only true) pos)
          "--target-file" (recur (rest (rest args)) (assoc opts :target-file (second args)) pos)
          (recur (rest args) opts (conj pos a)))))))

(defn- root [opts]
  (or (:root opts) (System/getProperty "user.dir")))

(defn- show [opts v]
  (if (:json opts)
    (println (json/write v))
    (pp/pprint v)))

(defn- pos-args [opts]
  (let [[file line col & more] (:pos opts)]
    {:file file
     :line (some-> line parse-long)
     :col (some-> col parse-long)
     :rest more}))

(defn run [args]
  (let [opts (parse-args args)
        cmd (:cmd opts)
        r (root opts)
        {:keys [file line col rest]} (pos-args opts)
        usage (fn []
                (println "usage: jolt-lsp <command> [options]")
                (println "commands: index refresh search def refs hover symbols diagnostics calls impact")
                (println "          deps form context check rename clean-ns add-require replace-form")
                (println "          insert-form refactor format new-file-ns ignored-forms structural-diff")
                (println "          rename-ns fix-declares extract deps-list deps-grep deps-read lsp mcp")
                (println "options:  --json --root DIR --apply --all --kondo --no-kondo --limit N")
                (println "          --direction D --alias A --old FORM --new TEXT --after FORM")
                (println "          --before FORM --occurrence N --every --fix --code TEXT --target-file F")
                (println "          --rev REV --to-ns NS --library L --glob G --aliases A,B")
                (println "          --ignore-case --files-only --offset N"))]
    (cond
      (= cmd "index") (show opts (do (when (:refresh opts) (api/refresh! r))
                                     (assoc (api/index-status r) :command "index")))
      (= cmd "refresh") (show opts (do (api/refresh! r) (api/index-status r)))
      (= cmd "search") (show opts (api/search r {:query (first (:pos opts)) :limit (:limit opts)}))
      (= cmd "def") (show opts (api/definition r {:file file :line line :col col}))
      (= cmd "refs") (show opts (if (and file line col)
                                  (api/references r {:file file :line line :col col})
                                  (api/references r {:symbol (first (:pos opts))})))
      (= cmd "check") (show opts (api/check r {:file file :code (:code opts) :fix (:fix opts)}))
      (= cmd "hover") (show opts (if (and file line col)
                                   (api/hover r {:file file :line line :col col})
                                   (api/hover r {:symbol (first (:pos opts))})))
      (= cmd "symbols") (show opts (api/document-symbols r {:file file}))
      (= cmd "diagnostics") (show opts (api/diagnostics* r {:file file :all (:all opts) :kondo (:kondo opts)}))
      (= cmd "calls") (show opts (api/call-hierarchy r {:symbol (first (:pos opts))
                                                        :direction (or (:direction opts) :callers)}))
      (= cmd "impact") (show opts (api/impact r {:symbol (first (:pos opts))}))
      (= cmd "deps") (show opts (api/deps r {:ns (first (:pos opts))
                                             :direction (or (:direction opts) :dependencies)}))
      (= cmd "deps-list") (show opts (api/deps-list r {:pattern (first (:pos opts))
                                                       :aliases (:aliases opts)}))
      (= cmd "deps-grep") (show opts (api/deps-grep r (assoc (select-keys opts [:library :glob :aliases :limit
                                                                                :case-insensitive :files-only])
                                                             :pattern (first (:pos opts)))))
      (= cmd "deps-read") (show opts (api/deps-read r (merge (select-keys opts [:library :aliases :offset :limit])
                                                             (if (:library opts)
                                                               {:path (first (:pos opts))}
                                                               {:file (first (:pos opts))}))))
      (= cmd "form") (show opts (api/form r {:file file :line line :col col}))
      (= cmd "context") (show opts (if (and file (string/includes? file "."))
                                     (if (re-find #"\.(clj|cljc|edn)$" file)
                                       (api/context r {:file file})
                                       (api/context r {:ns file}))
                                     (api/context r {:ns file})))
      (= cmd "new-file-ns") (show opts (api/new-file-ns r {:file file}))
      (= cmd "ignored-forms") (show opts (api/ignored-forms r {:file file}))
      (= cmd "rename") (show opts (api/rename* r {:path file :row line :col col
                                                  :to (first rest) :apply (:apply opts)}))
      (= cmd "clean-ns") (show opts (api/clean-ns* r {:file file :apply (:apply opts)}))
      (= cmd "add-require") (show opts (api/add-require* r {:file file :symbol (first rest)
                                                            :alias (:alias opts) :apply (:apply opts)}))
      (= cmd "replace-form") (show opts (api/replace-form* r {:file file :old-form (:old-form opts)
                                                              :new-text (:new-text opts)
                                                              :occurrence (:occurrence opts)
                                                              :all (:every opts)
                                                              :apply (:apply opts)}))
      (= cmd "insert-form") (show opts (api/insert-form* r {:file file :after (:after opts)
                                                            :before (:before opts)
                                                            :occurrence (:occurrence opts)
                                                            :new-text (:new-text opts)
                                                            :apply (:apply opts)}))
      (= cmd "format") (show opts (api/format* r {:file file :apply (:apply opts)}))
      (= cmd "refactor") (let [[kind file* line* col*] (:pos opts)]
                           (show opts (api/refactor* r {:kind kind
                                                        :file file*
                                                        :line (when line* (read-string line*))
                                                        :col (when col* (read-string col*))
                                                        :target-file (:target-file opts)
                                                        :apply (:apply opts)})))
      (= cmd "structural-diff") (show opts (api/structural-diff r {:file file :rev (:rev opts)}))
      (= cmd "rename-ns") (show opts (api/rename-namespace* r {:from (first (:pos opts)) :to (second (:pos opts))
                                                               :apply (:apply opts)}))
      (= cmd "fix-declares") (show opts (api/fix-declares* r {:file file :apply (:apply opts)}))
      (= cmd "extract") (show opts (api/extract* r {:file file :forms (vec (drop 1 (:pos opts)))
                                                    :to-ns (:to-ns opts) :apply (:apply opts)}))
      (contains? #{"help" "--help" "-h"} cmd) (usage)
      :else (do (usage) (System/exit (if cmd 1 0))))))

(defn -main [& args]
  (try
    (run args)
    ;; analysis runs on futures: let their pool go
    (shutdown-agents)
    (System/exit 0)
    (catch Throwable e
      (binding [*out* *err*]
        (println (json/write {:ok false :error (str e)})))
      (System/exit 1))))
