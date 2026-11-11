(ns jolt-lsp.custom
  "Custom linters: project files that define a function taking one context map."
  (:require [babashka.fs :as fs]
            [jolt-lsp.db :as db]))

(defonce loaded (atom #{}))

(defn- load-file-once! [path]
  (when-not (contains? @loaded path)
    (load-file path)
    (swap! loaded conj path))
  path)

(defn- resolve-fn [sym]
  (when (and sym (namespace sym))
    (when-let [n (find-ns (symbol (namespace sym)))]
      (ns-resolve n (symbol (name sym))))))

(defn- normalize [path d]
  (merge {:file path :severity :warning :source :custom
          :code :custom :message "custom linter finding"
          :row 1 :col 1 :end-row 1 :end-col 1}
         d))

(defn lint [db root path opts]
  (let [file (db/file-by-path db path)
        fid (:id file)
        ctx {:db db
             :file path
             :ns (:ns file)
             :var-defs (db/var-defs-in-file db fid)
             :var-refs (db/var-refs-in-file db fid)
             :local-defs (db/local-defs-in-file db fid)
             :local-refs (db/local-refs-in-file db fid)}]
    (vec
     (mapcat
      (fn [{:keys [file fn]}]
        (try
          (let [p (str (fs/path root file))
                _ (load-file-once! p)
                f (resolve-fn (symbol fn))]
            (if f
              (map #(normalize path %) (f ctx))
              [{:file path :severity :error :source :custom :code :custom-linter-missing
                :message (str "custom linter not found: " fn)
                :row 1 :col 1 :end-row 1 :end-col 1}]))
          (catch Throwable e
            [{:file path :severity :error :source :custom :code :custom-linter-error
              :message (str "custom linter failed: " e)
              :row 1 :col 1 :end-row 1 :end-col 1}])))
      (:custom-linters opts)))))
