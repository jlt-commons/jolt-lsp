(ns jolt-lsp.api
  "The engine surface shared by the CLI, MCP, and LSP transports."
  (:require [clojure.string :as string]
            [jolt-lsp.db :as db]
            [jolt-lsp.diagnostics :as diagnostics]
            [jolt-lsp.edit.add-require :as add-require]
            [jolt-lsp.edit.clean-ns :as clean-ns]
            [jolt-lsp.edit.rename :as rename]
            [jolt-lsp.edit.vedit :as vedit]
            [jolt-lsp.forms :as forms]
            [jolt-lsp.generate]
            [jolt-lsp.ignored]
            [jolt-lsp.graph.query :as graph]
            [jolt-lsp.paths :as paths]
            [jolt-lsp.query :as query]
            [jolt-lsp.refactor :as refactor]
            [jolt-lsp.text :as text]))

(defonce state (atom {:root nil :db nil}))

(defn root-for [root]
  (or root (System/getProperty "user.dir")))

(defn ensure-db [root]
  (let [root (root-for root)]
    (if (and (= root (:root @state)) (:db @state))
      (:db @state)
      (let [d (db/build-cached root)]
        (reset! state {:root root :db d})
        d))))

(defn refresh! [root]
  (let [root (root-for root)
        d (db/build-cached root)]
    (reset! state {:root root :db d})
    d))

(defn refresh-file!
  ([root path] (refresh-file! root path nil))
  ([root path source]
   (let [root (root-for root)
         d (if source
             (db/update-file-text (:db @state) root path source)
             (db/update-file (:db @state) root path))]
     (swap! state assoc :root root :db d)
     d)))

(defn index-status [root]
  (let [root (root-for root)
        d (ensure-db root)
        files (db/all-files d)
        vars (db/all-var-defs d)]
    {:root root
     :files (count files)
     :namespaces (count (distinct (keep :ns files)))
     :vars (count vars)
     :keywords (count (db/all-keyword-defs d))}))

(defn- position->lsp [source row col]
  {:line (dec row)
   :character (text/codepoint->utf16-col source row (dec col))})

(defn- offsets [source [r c er ec]]
  (let [start (text/position->offset source [r c])
        end (text/position->offset source [er ec])]
    {:start start :end end
     :start-byte (text/utf8-offset source start)
     :end-byte (text/utf8-offset source end)}))

(defn location->external [loc]
  (let [path (:path loc)
        source (try (slurp path) (catch Throwable _ ""))
        r (:range loc)]
    (if (nil? r)
      loc
      (let [{:keys [start end start-byte end-byte]} (offsets source r)
            [sr sc er ec] r]
        (assoc loc
               :file path
               :line sr :col sc :end-line er :end-col ec
               :bytes [start-byte end-byte]
               :lsp {:start (position->lsp source sr sc)
                     :end (position->lsp source er ec)}
               :source (first (string/split-lines
                               (subs source start (min (count source)
                                                       (+ start 200))))))))))

(defn definition
  [root opts]
  (let [d (ensure-db root)]
    (if-let [fqn (:symbol opts)]
      (if-let [v (db/var-by-fqn d fqn)]
        [(location->external (query/var-location d v))]
        [])
      (mapv location->external
            (query/definition-at d (:file opts) (:line opts) (:col opts))))))

(defn references [root opts]
  (let [d (ensure-db root)]
    (mapv location->external (query/references d (:symbol opts)))))

(defn search [root opts]
  (let [d (ensure-db root)]
    (mapv location->external
          (query/search d (:query opts) (or (:limit opts) 50)))))

(defn hover [root opts]
  (let [d (ensure-db root)]
    (if-let [fqn (:symbol opts)]
      (some-> (query/hover d fqn) location->external)
      (let [locs (query/definition-at d (:file opts) (:line opts) (:col opts))]
        (some-> (first locs) :fqn (query/hover d) location->external)))))

(defn document-symbols [root opts]
  (let [d (ensure-db root)]
    (query/document-symbols d (:file opts))))

(defn diagnostics* [root opts]
  (let [d (ensure-db root)]
    (if (:all opts)
      (vec (diagnostics/project-diagnostics d {:kondo (:kondo opts)}))
      (vec (diagnostics/file-diagnostics d (:file opts) {:kondo (:kondo opts)})))))

(defn- direction [x default]
  (if x (keyword (name x)) default))

(defn call-hierarchy [root opts]
  (let [d (ensure-db root)]
    (graph/call-hierarchy d (:symbol opts)
                          (direction (:direction opts) :callers))))

(defn impact [root opts]
  (let [d (ensure-db root)]
    (vec (sort (graph/impact d (:symbol opts))))))

(defn deps [root opts]
  (let [d (ensure-db root)
        ns (:ns opts)
        direction (direction (:direction opts) :dependencies)]
    {:ns ns
     :direction direction
     :nodes (vec (sort (case direction
                         :dependents (graph/ns-dependents d ns)
                         (graph/ns-dependencies d ns))))}))

(defn form [root opts]
  (let [path (:file opts)
        source (slurp path)]
    (if (= :top (direction (:mode opts) :enclosing))
      (mapv (fn [f] {:text (:text f) :range (:range f)})
            (forms/top-level-forms source))
      (forms/form-at source (:line opts) (:col opts)))))

(defn context [root opts]
  (let [d (ensure-db root)
        ns (or (:ns opts)
               (when (:file opts) (some-> (db/file-by-path d (:file opts)) :ns)))]
    (if ns
      (query/context d ns)
      {:ok false :error "pass ns or file"})))

(defn- edit-view [e]
  (select-keys e [:start-byte :end-byte :start-row :start-col
                  :end-row :end-col :old-text :new-text]))

(defn- preview-map [result]
  (-> result
      (update :edits (fn [es]
                       (into {}
                             (map (fn [[path edits]] [path (mapv edit-view edits)]))
                             es)))
      (update :preview (fn [files]
                         (into {}
                               (map (fn [[path text]]
                                      [path (if (> (count text) 4000)
                                              (subs text 0 4000)
                                              text)]))
                               files)))))

(defn- one-file-result [result]
  (-> result
      (update :edits (fn [es] (mapv edit-view es)))
      (update :preview (fn [text]
                         (if (and (string? text) (> (count text) 4000))
                           (subs text 0 4000)
                           text)))))

(defn rename* [root opts]
  (let [d (ensure-db root)
        r (rename/rename d opts)]
    (if (and (:ok r) (:apply opts))
      (do (rename/apply! r) (preview-map r))
      (preview-map r))))

(defn clean-ns* [root opts]
  (let [d (ensure-db root)
        r (clean-ns/plan d (:file opts))]
    (if (and (:ok r) (:apply opts) (:changed r))
      (do (spit (:file opts) (:preview r)) (one-file-result r))
      (one-file-result r))))

(defn add-require* [root opts]
  (let [d (ensure-db root)
        r (add-require/plan d opts)]
    (if (and (:ok r) (:apply opts) (:changed r))
      (do (spit (:file opts) (:preview r)) (one-file-result r))
      (one-file-result r))))

(defn replace-form* [root opts]
  (let [source (slurp (:file opts))
        edit (if (:old-form opts)
               (forms/replace-form source (:old-form opts) (:new-text opts))
               (when (:range opts)
                 (vedit/range->edit source (:range opts) (:new-text opts))))]
    (if edit
      (let [new (vedit/apply-edits source [edit])]
        (if (:apply opts) (spit (:file opts) new))
        {:ok true :changed true
         :edits {(:file opts) [(select-keys edit [:start-byte :end-byte
                                                  :start-row :start-col
                                                  :end-row :end-col
                                                  :old-text :new-text])]}
         :preview {(:file opts) (if (> (count new) 4000) (subs new 0 4000) new)}})
      {:ok false :reason "form not found"})))

(defn insert-form* [root opts]
  (let [source (slurp (:file opts))
        edit (forms/insert-after source (:after opts) (:new-text opts))]
    (if edit
      (let [new (vedit/apply-edits source [edit])]
        (if (:apply opts) (spit (:file opts) new))
        {:ok true :changed true
         :edits {(:file opts) [(select-keys edit [:start-byte :end-byte
                                                  :start-row :start-col
                                                  :end-row :end-col
                                                  :old-text :new-text])]}
         :preview {(:file opts) (if (> (count new) 4000) (subs new 0 4000) new)}})
      {:ok false :reason "anchor form not found"})))

(defn new-file-ns [root opts]
  (let [path (:file opts)
        ns (jolt-lsp.generate/ns-for-path (root-for root) path)]
    {:ns ns :form (jolt-lsp.generate/ns-form ns)}))

(defn ignored-forms [root opts]
  (jolt-lsp.ignored/ranges (slurp (:file opts))))


(defn refactor* [root opts]
  (let [d (ensure-db root)
        r (case (keyword (:kind opts))
            :sort-requires (refactor/sort-requires d (:file opts))
            :cycle-keyword (refactor/cycle-keyword d (:file opts) (:line opts) (:col opts))
            :inline-symbol (refactor/inline-symbol d (:file opts) (:line opts) (:col opts))
            :destructure-keys (refactor/destructure-keys d (:file opts) (:line opts) (:col opts))
            :move-form (refactor/move-form d (:file opts) (:line opts) (:col opts)
                                           (:target-file opts))
            {:ok false :reason (str "unknown refactor " (:kind opts))})]
    (if (and (:ok r) (:apply opts))
      (do (doseq [[p text] (:preview r)] (spit p text)) r)
      r)))
