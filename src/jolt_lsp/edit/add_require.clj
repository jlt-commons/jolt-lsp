(ns jolt-lsp.edit.add-require
  "Add a :require for a symbol an agent wants to use.

  The result says how to write the symbol afterwards (:use \"alias/name\"),
  so an agent can add the require and the call in one step."
  (:require [clojure.string :as string]
            [jolt-lsp.db :as db]
            [jolt-lsp.edit.vedit :as vedit]
            [jolt-lsp.forms :as forms]
            [jolt-lsp.tree :as tree]))

(defn- last-segment [ns]
  (last (string/split ns #"\.")))

(defn- candidates
  "Public vars named `name` in namespaces other than `own-ns`."
  [db name own-ns]
  (->> (db/all-var-defs db)
       (filter #(and (= (:name %) name) (not (:private %)) (not= (:ns %) own-ns)))
       (sort-by (fn [v] [(if (:external (db/file-by-id db (:file-id v))) 1 0) (:fqn v)]))
       vec))

(defn- insertion
  "The edit that adds `libspec` text to the ns form of source."
  [source libspec]
  (let [{ns-node :ns clause :clause libspecs :libspecs} (forms/ns-parts source)]
    (cond
      (and clause (seq libspecs))
      (let [last-ls (last libspecs)
            [_ col] (tree/node-range (first libspecs))
            [_ _ er ec] (tree/node-range last-ls)]
        (vedit/range->edit source [er ec er ec]
                           (str "\n" (apply str (repeat (dec col) " ")) libspec)))

      clause
      (let [[_ _ er ec] (tree/node-range clause)]
        (vedit/range->edit source [er (dec ec) er (dec ec)] (str " " libspec)))

      ns-node
      (let [[_ col er ec] (tree/node-range ns-node)]
        (vedit/range->edit source [er (dec ec) er (dec ec)]
                           (str "\n" (apply str (repeat (inc col) " "))
                                "(:require " libspec ")")))

      :else nil)))

(defn plan
  "opts: :path, :symbol (ns/name, alias-free fqn, or a bare name to look
  up), :alias. `source` defaults to the file on disk."
  ([db opts] (plan db opts nil))
  ([db {:keys [path symbol alias]} source]
   (if-let [file (db/file-by-path db path)]
     (let [own-ns (:ns file)
           aliases (into {} (map (juxt :alias :target)) (db/file-aliases db path))
           alias-of (into {} (map (juxt :target :alias)) (db/file-aliases db path))
           requires (set (map :target (db/file-requires db path)))
           slash (string/index-of symbol "/")
           prefix (when slash (subs symbol 0 slash))
           name (if slash (subs symbol (inc slash)) symbol)
           found (when-not slash (candidates db name own-ns))
           target (cond
                    prefix (get aliases prefix prefix)
                    (= 1 (count found)) (:ns (first found)))]
       (cond
         (and (nil? prefix) (empty? found))
         {:ok false :reason (str "no public var named " name " in the index")}

         (nil? target)
         {:ok false :reason (str "ambiguous: " name " is defined in several namespaces; pass one")
          :candidates (mapv :fqn found)}

         (= target own-ns)
         {:ok false :reason (str symbol " is in this namespace; no require needed")
          :use name}

         (or (contains? requires target) (contains? aliases prefix))
         (let [a (get alias-of target)]
           {:ok true :changed false :reason "namespace already required"
            :alias a :use (str (or a target) "/" name)})

         :else
         (let [alias (or alias (last-segment target))]
           (if (and (contains? aliases alias) (not= target (get aliases alias)))
             {:ok false
              :reason (str "alias " alias " already names " (get aliases alias) "; pass another alias")}
             (let [source (or source (slurp path))
                   edit (insertion source (str "[" target " :as " alias "]"))]
               (if edit
                 {:ok true :path path :changed true :edits [edit]
                  :alias alias :use (str alias "/" name)
                  :preview (vedit/apply-edits source [edit])}
                 {:ok false :reason "no ns form"}))))))
     {:ok false :reason (str "not an indexed file: " path)})))

(defn apply! [result]
  (when (and (:ok result) (:changed result))
    (spit (:path result) (:preview result))))
