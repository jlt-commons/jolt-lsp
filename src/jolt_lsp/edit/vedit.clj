(ns jolt-lsp.edit.vedit
  "Range edits and their application. Offsets are codepoints internally;
  byte offsets are UTF-8 and are what agents patch with."
  (:require [jolt-lsp.text :as text]))

(defn range->edit [source [sr sc er ec] new-text]
  (let [start (text/position->offset source [sr sc])
        end (text/position->offset source [er ec])]
    {:start-start sr :start-col sc :end-row er :end-col ec
     :start-offset start
     :end-offset end
     :start-byte (text/utf8-offset source start)
     :end-byte (text/utf8-offset source end)
     :old-text (subs source start end)
     :new-text new-text}))

(defn apply-edits [source edits]
  (reduce (fn [s e]
            (str (subs s 0 (:start-offset e))
                 (:new-text e)
                 (subs s (:end-offset e))))
          source
          (sort-by :start-offset > edits)))

