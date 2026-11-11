(ns jolt-lsp.edit.add-require
  "Add a :require for a symbol an agent wants to use."
  (:require [clojure.string :as string]
            [jolt-lsp.db :as db]
            [jolt-lsp.edit.vedit :as vedit]
            [jolt-lsp.forms :as forms]
            [jolt-lsp.tree :as tree]
            [relatum.core :as rc]))

(defn- last-segment [ns]
  (last (string/split ns #"\.")))

(defn- var-by-name [db name]
  (->> (rc/rows db :var-def)
       (filter #(= (:name %) name))
       vec))

(defn plan [db {:keys [path symbol alias]}]
  (when-let [file (db/file-by-path db path)]
    (let [ns (:ns file)
          aliases (into {} (map (juxt :alias :target))
                        (or (db/ns-aliases-of db ns) []))
          requires (set (map :target (or (db/ns-requires-of db ns) [])))
          slash (string/index-of symbol "/")
          prefix (when slash (subs symbol 0 slash))
          name (if slash (subs symbol (inc slash)) symbol)
          qualified? (some? slash)]
      (cond
        (and qualified? (or (contains? aliases prefix) (contains? requires prefix)))
        {:ok true :changed false :reason "namespace already required"}

        qualified?
        (let [target prefix
              alias (or alias (last-segment target))
              source (slurp path)
                clause (forms/ns-require-clause source)
                ns-node (forms/ns-form source)
                insert-range (cond
                               clause [(nth (tree/node-range clause) 2)
                                       (dec (nth (tree/node-range clause) 3))
                                       (nth (tree/node-range clause) 2)
                                       (dec (nth (tree/node-range clause) 3))]
                               ns-node [(nth (tree/node-range ns-node) 2)
                                        (dec (nth (tree/node-range ns-node) 3))
                                        (nth (tree/node-range ns-node) 2)
                                        (dec (nth (tree/node-range ns-node) 3))]
                               :else nil)
                indent (if clause (+ 2 (nth (tree/node-range clause) 1)) 2)
                text (str "\n" (apply str (repeat indent " "))
                          "[" target " :as " alias "]")
                edit (when insert-range (vedit/range->edit source insert-range text))]
          (if edit
            {:ok true :path path :changed true :edits [edit]
             :preview (vedit/apply-edits source [edit])}
            {:ok false :reason "no ns form"}))

        :else
        (let [vs (var-by-name db name)]
          (if (= 1 (count vs))
            (let [target (:ns (first vs))
                  alias (or alias (last-segment target))
                  source (slurp path)
                  clause (forms/ns-require-clause source)
                  ns-node (forms/ns-form source)
                  r (or (some-> clause tree/node-range)
                        (some-> ns-node tree/node-range))
                  insert-range (when r [(nth r 2) (dec (nth r 3))
                                        (nth r 2) (dec (nth r 3))])
                  indent (if clause (+ 2 (nth r 1)) 2)
                  text (str "\n" (apply str (repeat indent " "))
                            "[" target " :as " alias "]")
                  edit (when insert-range (vedit/range->edit source insert-range text))]
              (if edit
                {:ok true :path path :changed true :edits [edit]
                 :preview (vedit/apply-edits source [edit])}
                {:ok false :reason "no ns form"}))
            {:ok false :reason (str "ambiguous or missing var " name)}))))))

(defn apply! [result]
  (when (and (:ok result) (:changed result))
    (spit (:path result) (:preview result))))
