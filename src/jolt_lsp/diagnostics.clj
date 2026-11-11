(ns jolt-lsp.diagnostics
  "Built-in findings for one file, computed from the index."
  (:require [clojure.string :as string]
            [jolt-lsp.db :as db]
            [jolt-lsp.kondo :as kondo]))

(defn- diag [r severity code message]
  {:row (nth r 0) :col (nth r 1)
   :end-row (nth r 2) :end-col (nth r 3)
   :severity severity
   :code code
   :message message
   :source :jolt-lsp})

(defn- ns-name-of [db fid]
  (:ns (db/file-by-id db fid)))

(defn- used-prefix? [refs prefix]
  (some (fn [r] (and (:fqn r) (or (= (:prefix r) prefix)
                                  (string/starts-with? (:fqn r) (str prefix "/")))))
        refs))

(defn- refer-used? [refs refer]
  (some (fn [r] (and (= (:fqn r) (:target refer))
                     (= (:text r) (:name refer))))
        refs))

(defn unresolved-namespace [refs]
  (for [r refs
        :when (and (:qualified r) (:unresolved r) (:prefix r))]
    (diag [(:row r) (:col r) (:end-row r) (:end-col r)]
          :error :unresolved-namespace
          (str "Unknown namespace: " (:prefix r)))))

(defn unresolved-symbol [refs]
  (for [r refs
        :when (and (not (:qualified r)) (:unresolved r)
                   (not (string/starts-with? (str (:text r)) ".")))]
    (diag [(:row r) (:col r) (:end-row r) (:end-col r)]
          :error :unresolved-symbol
          (str "Unresolved symbol: " (:text r)))))

(defn unused-namespaces [requires refers refs]
  (for [req requires
        :let [alias (:alias req)
              target (:target req)]
        :when (and target
                   (not (used-prefix? refs target))
                   (not (and alias (used-prefix? refs alias)))
                   (not (some #(refer-used? refs %) (filter #(= (:target %) target) refers))))]
    (diag [(:row req) (:col req) (:end-row req) (:end-col req)]
          :warning :unused-namespace
          (str "Unused namespace: " target))))

(defn duplicate-requires [requires]
  (->> requires
       (group-by :target)
       (keep (fn [[target rs]]
               (when (> (count rs) 1)
                 (let [r (first (sort-by :row rs))]
                   (diag [(:row r) (:col r) (:end-row r) (:end-col r)]
                         :warning :duplicate-require
                         (str "Duplicate require: " target))))))))

(defn unused-bindings [locals local-refs]
  (let [used (set (map :local-id local-refs))]
    (for [l locals
          :when (and (not (contains? used (:id l)))
                     (not= :field (:kind l))
                     (not= "_" (:name l))
                     (not (string/starts-with? (:name l) "_")))]
      (diag [(:row l) (:col l) (:end-row l) (:end-col l)]
            :warning :unused-binding
            (str "Unused binding: " (:name l))))))

(defn unused-private-vars [var-defs var-refs]
  (let [used (set (map :fqn var-refs))]
    (for [v var-defs
          :when (and (:private v)
                     (not= :declare (:kind v))
                     (not= :protocol-method (:kind v))
                     (not (contains? used (:fqn v))))]
      (diag [(:name-row v) (:name-col v) (:name-end-row v) (:name-end-col v)]
            :warning :unused-private-var
            (str "Unused private var: " (:name v))))))

(defn file-diagnostics
  ([db path] (file-diagnostics db path {}))
  ([db path opts]
  (when-let [file (db/file-by-path db path)]
    (let [fid (:id file)
          ns (ns-name-of db fid)
          refs (vec (db/var-refs-in-file db fid))
          requires (vec (or (db/ns-requires-of db ns) []))
          refers (vec (or (db/ns-refers-of db ns) []))
          locals (vec (db/local-defs-in-file db fid))
          local-refs (vec (db/local-refs-in-file db fid))
          var-defs (vec (db/var-defs-in-file db fid))]
      (->> (concat (unresolved-namespace refs)
                   (unresolved-symbol refs)
                   (unused-namespaces requires refers refs)
                   (duplicate-requires requires)
                   (unused-bindings locals local-refs)
                   (unused-private-vars var-defs refs)
                   (when (:kondo opts) (kondo/lint [path])))
           (map #(assoc % :file path))
           (reduce (fn [m d]
                     (assoc m [(:file d) (:row d) (:col d) (:code d)] d))
                   {})
           vals
           vec)))))

(defn project-diagnostics
  ([db] (project-diagnostics db {}))
  ([db opts]
   (mapcat (fn [f] (file-diagnostics db (:path f) opts)) (db/all-files db))))
