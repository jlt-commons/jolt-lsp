(ns jolt-lsp.edit.io
  "Writing edits to disk."
  (:require [jolt-lsp.edit.vedit :as vedit]))

(defn apply-files [edits-by-file]
  (reduce-kv
   (fn [acc path edits]
     (let [old (slurp path)
           new (vedit/apply-edits old edits)]
       (assoc-in acc [:files path] new)))
   {:files {} :edits edits-by-file}
   edits-by-file))

(defn write-files! [{:keys [files]}]
  (doseq [[path text] files] (spit path text))
  files)
