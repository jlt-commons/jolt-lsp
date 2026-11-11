(ns jolt-lsp.edit.clean-ns
  "Drop unused and duplicate requires, preserving everything else."
  (:require [clojure.string :as string]
            [jolt-lsp.db :as db]
            [jolt-lsp.edit.vedit :as vedit]
            [jolt-lsp.forms :as forms]
            [jolt-lsp.tree :as tree]))

(defn- used-prefix? [refs prefix]
  (some (fn [r] (and (:fqn r) (or (= (:prefix r) prefix)
                                  (string/starts-with? (:fqn r) (str prefix "/")))))
        refs))

(defn- refer-used? [refs refer]
  (some (fn [r] (and (= (:fqn r) (:target refer))
                     (= (:text r) (:name refer))))
        refs))

(defn- used? [requires refers refs req]
  (let [alias (:alias req)
        target (:target req)]
    (or (used-prefix? refs target)
        (and alias (used-prefix? refs alias))
        (some #(refer-used? refs %) (filter #(= (:target %) target) refers)))))

(defn plan [db path]
  (when-let [file (db/file-by-path db path)]
    (let [ns (:ns file)
          requires (vec (or (db/ns-requires-of db ns) []))
          refers (vec (or (db/ns-refers-of db ns) []))
          refs (vec (db/var-refs-in-file db (:id file)))
          by-target (group-by :target requires)
          duplicates (for [[_ rs] by-target
                           r (rest (sort-by :row rs))]
                       r)
          unused (remove #(used? requires refers refs %) requires)
          remove-rows (set (map :id (concat unused duplicates)))
          source (slurp path)
          clause (forms/ns-require-clause source)
          libspecs (forms/ns-libspecs source)
          remove-ranges (distinct (for [ls libspecs
                              :let [r (tree/node-range ls)]
                              :when (some (fn [row]
                                            (= r [(:row row) (:col row) (:end-row row) (:end-col row)]))
                                          (filter #(contains? remove-rows (:id %)) requires))]
                          r))
          clause-range (when clause (tree/node-range clause))
          all-libspecs? (and (seq libspecs) (= (count remove-ranges) (count libspecs)))
          ranges (if all-libspecs? [clause-range] remove-ranges)
          edits (mapv #(vedit/range->edit source % "") ranges)]
      {:ok true
       :path path
       :changed (boolean (seq edits))
       :edits edits
       :preview (vedit/apply-edits source edits)
       :removed (vec remove-rows)})))

(defn apply! [result]
  (when (and (:ok result) (:changed result))
    (spit (:path result) (:preview result))))
