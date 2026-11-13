(ns jolt-lsp.api
  "The engine surface shared by the CLI, MCP, and LSP transports.

  Every call works on a fresh index: files changed on disk since they were
  indexed (by an agent's own edit tool, say) are re-analyzed first, and an
  applied edit re-indexes what it wrote. Paths may be relative to the root.

  Edits preview by default and come back as a diff. An edit that would
  leave a file that read before unreadable is refused, not written."
  (:require [babashka.fs :as fs]
            [clojure.string :as string]
            [jolt-lsp.db :as db]
            [jolt-lsp.dep-source :as dep-source]
            [jolt-lsp.diagnostics :as diagnostics]
            [jolt-lsp.edit.add-require :as add-require]
            [jolt-lsp.edit.clean-ns :as clean-ns]
            [jolt-lsp.edit.diff :as diff]
            [jolt-lsp.edit.rename :as rename]
            [jolt-lsp.edit.vedit :as vedit]
            [jolt-lsp.format :as fmt]
            [jolt-lsp.forms :as forms]
            [jolt-lsp.generate]
            [jolt-lsp.git :as git]
            [jolt-lsp.graph.query :as graph]
            [jolt-lsp.ignored]
            [jolt-lsp.kondo :as kondo]
            [jolt-lsp.query :as query]
            [jolt-lsp.refactor :as refactor]
            [jolt-lsp.replace :as replace]
            [jolt-lsp.sdiff.core :as sdiff]
            [jolt-lsp.sdiff.edn :as sdiff-edn]
            [jolt-lsp.sdiff.group :as sdiff-group]
            [jolt-lsp.sdiff.text :as sdiff-text]
            [jolt-lsp.settings :as settings]
            [jolt-lsp.surgeon.extract :as extract]
            [jolt-lsp.surgeon.fix-declares :as fix-declares]
            [jolt-lsp.surgeon.rename :as ns-rename]
            [jolt-lsp.syntax :as syntax]
            [jolt-lsp.text :as text]
            [jolt-lsp.text.index :as tix]))

(defonce state (atom {:root nil :db nil :synced 0}))

(def ^:private sync-interval-ms
  "Minimum gap between two scans of the project for changed files."
  250)

(defn root-for [root]
  (let [r (or root (System/getProperty "user.dir"))]
    (try (str (fs/canonicalize r)) (catch Throwable _ (str r)))))

(defn resolve-path
  "An absolute, normalized path for `file`, relative paths taken from root."
  [root file]
  (when file
    (db/normalize-path (if (fs/absolute? file) file (str (fs/path (root-for root) file))))))

(defn- sync-stale
  "Re-index project files changed, added or deleted on disk."
  [root d]
  (let [{:keys [changed added deleted]} (db/stale-files d root)
        d (reduce #(db/update-file %1 root %2) d (concat changed added))]
    (reduce #(db/delete-file %1 root %2) d deleted)))

(defn ensure-db
  "The index for root, built on first use and kept in sync with the disk."
  [root]
  (let [root (root-for root)
        {:keys [db synced] :as s} @state]
    (cond
      ;; a new root, or a lint config change, which every analysis depends on
      (not (and (= root (:root s)) db (= (:config s) (settings/config-stamp root))))
      (let [d (db/build-cached root)]
        (reset! state {:root root :db d :synced (System/currentTimeMillis)
                       :config (settings/config-stamp root)})
        d)

      (< (- (System/currentTimeMillis) synced) sync-interval-ms)
      db

      :else
      (let [d (sync-stale root db)]
        (swap! state assoc :db d :synced (System/currentTimeMillis))
        d))))

(defn refresh! [root]
  (let [root (root-for root)
        _ (db/forget! root)
        d (db/build-cached root)]
    (reset! state {:root root :db d :synced (System/currentTimeMillis)
                   :config (settings/config-stamp root)})
    d))

(defn refresh-file!
  "Re-index one file, from `source` when given (an editor buffer), else
  from disk."
  ([root path] (refresh-file! root path nil))
  ([root path source]
   (let [root (root-for root)
         d (ensure-db root)
         d (if source
             (db/update-file-text d root path source)
             (db/update-file d root path))]
     (swap! state assoc :root root :db d)
     d)))

(defn index-status [root]
  (let [root (root-for root)
        d (ensure-db root)
        files (db/all-files d)
        project (remove :external files)
        vars (db/all-var-defs d)]
    {:root root
     :files (count project)
     :dependency-files (- (count files) (count project))
     :namespaces (count (distinct (keep :ns project)))
     :vars (count vars)
     :keywords (count (db/all-keyword-defs d))
     :unreadable (vec (keep #(when (:parse-error %) (:path %)) project))}))

;; ---------------------------------------------------------------------------
;; locations

(defn- position->lsp [source row col]
  {:line (dec row)
   :character (tix/codepoint->utf16-col source row (dec col))})

(defn- externalize
  "Add every view an agent or editor wants to each location. Each file is
  read once."
  [locs]
  (let [sources (atom {})
        source-of (fn [p]
                    (or (get @sources p)
                        (let [s (try (slurp p) (catch Throwable _ ""))]
                          (swap! sources assoc p s)
                          s)))]
    (mapv (fn [loc]
            (let [path (:path loc)
                  r (:range loc)]
              (if (or (nil? r) (nil? path))
                loc
                (let [source (source-of path)
                      [sr sc er ec] r
                      start (tix/position->offset source [sr sc])
                      end (tix/position->offset source [er ec])]
                  (-> loc
                      (dissoc :path)
                      (assoc :file path
                             :line sr :col sc :end-line er :end-col ec
                             :bytes [(tix/utf8-offset source start) (tix/utf8-offset source end)]
                             :lsp {:start (position->lsp source sr sc)
                                   :end (position->lsp source er ec)}
                             :source (first (string/split-lines
                                             (subs source (tix/position->offset source [sr 1])
                                                   (min (count source)
                                                        (+ (tix/position->offset source [sr 1]) 200)))))))))))
          locs)))

(defn location->external [loc]
  (first (externalize [loc])))

(defn- resolve-symbol
  "An fqn for what an agent typed: an fqn, or a bare name that names one
  var. Returns {:fqn f} or {:candidates [...]}."
  [d sym]
  (let [sym (string/trim (str sym))]
    (cond
      (db/var-by-fqn d sym) {:fqn sym}
      (string/index-of sym "/") {:fqn sym}
      :else (let [vs (filter #(= (:name %) sym) (db/all-var-defs d))]
              (if (= 1 (count vs))
                {:fqn (:fqn (first vs))}
                {:candidates (vec (sort (map :fqn vs)))})))))

(defn- with-symbol
  "Call (f fqn) for opts' :symbol, or explain why it is ambiguous."
  [d opts f]
  (let [{:keys [fqn candidates]} (resolve-symbol d (:symbol opts))]
    (if fqn
      (f fqn)
      {:ok false
       :reason (if (seq candidates)
                 (str (:symbol opts) " names several vars; pass one fqn")
                 (str "no var named " (:symbol opts)))
       :candidates candidates})))

;; ---------------------------------------------------------------------------
;; reads

(defn definition
  [root opts]
  (let [d (ensure-db root)]
    (if (:symbol opts)
      (with-symbol d opts
        (fn [fqn]
          (if-let [v (db/var-by-fqn d fqn)]
            (externalize [(query/var-location d v)])
            [])))
      (externalize (query/definition-at d (resolve-path root (:file opts))
                                        (:line opts) (:col opts))))))

(defn references [root opts]
  (let [d (ensure-db root)
        fqn-of (fn []
                 (if (:symbol opts)
                   (resolve-symbol d (:symbol opts))
                   {:fqn (:fqn (first (query/definition-at
                                       d (resolve-path root (:file opts))
                                       (:line opts) (:col opts))))}))
        {:keys [fqn candidates]} (fqn-of)]
    (cond
      fqn (externalize (query/references d fqn))
      (seq candidates) {:ok false :reason "ambiguous symbol; pass one fqn"
                        :candidates candidates}
      :else [])))

(defn search [root opts]
  (let [d (ensure-db root)]
    (externalize (query/search d (:query opts) (or (:limit opts) 50)))))

(defn hover [root opts]
  (let [d (ensure-db root)]
    (if (:symbol opts)
      (with-symbol d opts #(some-> (query/hover d %) location->external))
      (let [locs (query/definition-at d (resolve-path root (:file opts))
                                      (:line opts) (:col opts))]
        (some-> (first locs) :fqn (->> (query/hover d)) location->external)))))

(defn document-symbols [root opts]
  (let [d (ensure-db root)]
    (query/document-symbols d (resolve-path root (:file opts)))))

(defn- diag-opts [root opts]
  (let [cfg (settings/load-settings (root-for root))
        k (if (some? (:kondo opts))
            (boolean (:kondo opts))
            (let [v (:kondo cfg)] (if (nil? v) (kondo/available?) (boolean v))))]
    {:root (root-for root)
     :kondo k
     :source (:source opts)
     :custom-linters (:custom-linters cfg)
     :clj-depend (:clj-depend cfg)}))

(defn diagnostics* [root opts]
  (let [d (ensure-db root)
        o (diag-opts root opts)]
    (if (or (:all opts) (nil? (:file opts)))
      (vec (diagnostics/project-diagnostics d o))
      (vec (diagnostics/file-diagnostics d (resolve-path root (:file opts)) o)))))

(defn- direction [x default]
  (if x (keyword (name x)) default))

(defn call-hierarchy [root opts]
  (let [d (ensure-db root)]
    (with-symbol d opts
      #(graph/call-hierarchy d % (direction (:direction opts) :callers)))))

(defn impact [root opts]
  (let [d (ensure-db root)]
    (with-symbol d opts
      (fn [fqn]
        {:symbol fqn
         :callers (graph/impact-detail d fqn)
         :references (count (db/refs-by-fqn d fqn))}))))

(defn deps [root opts]
  (let [d (ensure-db root)
        ns (:ns opts)
        direction (direction (:direction opts) :dependencies)]
    (case direction
      :cycles {:direction :cycles :cycles (graph/ns-cycles d)}
      {:ns ns
       :direction direction
       :nodes (vec (sort (case direction
                           :dependents (graph/ns-dependents d ns)
                           (graph/ns-dependencies d ns))))})))

;; ---------------------------------------------------------------------------
;; dependency sources

(defn- aliases-of [opts]
  (let [a (:aliases opts)]
    (cond (nil? a) []
          (string? a) (remove string/blank? (string/split a #"[,\s]+"))
          :else (map str a))))

(defn- with-deps-errors [f]
  (try (f)
       (catch Throwable e
         {:ok false :error (or (ex-message e) (str e))})))

(defn deps-list
  "The libraries the project depends on, transitively: lib, version, kind
  and the directory their files are in. :pattern filters lib names."
  [root opts]
  (with-deps-errors
    (fn []
      (let [re (some->> (:pattern opts) (str "(?i)") re-pattern)
            libs (dep-source/libraries (root-for root) {:aliases (aliases-of opts)})]
        {:libraries (vec (cond->> (map #(dissoc % :root) libs)
                           re (filter #(re-find re (:lib %)))))}))))

(defn deps-grep
  "Regex search across the dependencies' files."
  [root opts]
  (with-deps-errors
    #(dep-source/grep (root-for root) (assoc opts :aliases (aliases-of opts)))))

(defn deps-read
  "Read a file, or a line range of one, inside a dependency."
  [root opts]
  (with-deps-errors
    #(dep-source/read-file (root-for root) (assoc opts :aliases (aliases-of opts)))))

(defn form [root opts]
  (let [path (resolve-path root (:file opts))
        source (slurp path)]
    (if (or (= :top (direction (:mode opts) :enclosing)) (nil? (:line opts)))
      (mapv (fn [f] {:text (:text f) :range (:range f)})
            (forms/top-level-forms source))
      (forms/form-at source (:line opts) (:col opts)))))

(defn context [root opts]
  (let [d (ensure-db root)
        path (some->> (:file opts) (resolve-path root))
        ns (or (:ns opts) (some-> path (->> (db/file-by-path d)) :ns))]
    (if ns
      (let [file (or path (:path (db/file-by-ns d ns)))]
        (merge (query/context d ns)
               (when file (query/file-ns-summary d file))))
      {:ok false :error "pass ns or file"})))

(defn new-file-ns [root opts]
  (let [path (resolve-path root (:file opts))
        ns (jolt-lsp.generate/ns-for-path (root-for root) path)]
    {:ns ns :form (jolt-lsp.generate/ns-form ns)}))

(defn ignored-forms [root opts]
  (jolt-lsp.ignored/ranges (slurp (resolve-path root (:file opts)))))

;; ---------------------------------------------------------------------------
;; syntax

(defn check
  "Diagnose, and where safe repair, Clojure text: a file or a code string.
  With :fix on a file, writes the repair."
  [root opts]
  (let [path (some->> (:file opts) (resolve-path root))
        text (cond path (slurp path)
                   (:code opts) (:code opts)
                   :else nil)]
    (if (nil? text)
      {:ok false :reason "pass file or code"}
      (let [r (syntax/balance text)
            note (syntax/explain r)
            base (cond-> {:ok (contains? #{:balanced :repaired} (:status r))
                          :status (:status r)}
                   note (assoc :note note)
                   (:reason r) (assoc :reason (:reason r))
                   (:line r) (assoc :line (:line r) :col (:col r))
                   (:open-line r) (assoc :open-line (:open-line r) :open-col (:open-col r)))]
        (cond
          (not= :repaired (:status r)) base

          (and path (:fix opts))
          (do (spit path (:content r))
              (when (:root @state) (refresh-file! root path))
              (assoc base :written true :file path))

          path (assoc base :written false :hint "pass fix=true to write the repair")

          :else (assoc base :code (:content r)))))))

;; ---------------------------------------------------------------------------
;; edits

(defn- whole-edit
  "One edit from old to new text, trimmed to the span that changed."
  [old new]
  (let [n (min (count old) (count new))
        pre (loop [i 0] (if (and (< i n) (= (nth old i) (nth new i))) (recur (inc i)) i))
        suf (loop [i 0] (if (and (< i (- n pre))
                                 (= (nth old (- (count old) 1 i)) (nth new (- (count new) 1 i))))
                          (recur (inc i)) i))]
    (vedit/offsets->edit old pre (- (count old) suf) (subs new pre (- (count new) suf)))))

(defn- edit-view [e]
  (select-keys e [:start-byte :end-byte :start-row :start-col
                  :end-row :end-col :old-text :new-text]))

(defn- broken-by
  "The problem the new text has when the old text read and the new does
  not, else nil."
  [old new]
  (when (and (nil? (syntax/diagnose old)) (string? new))
    (syntax/diagnose new)))

(defn- finish
  "Shape, check and optionally write an edit result. `r` is either
  multi-file ({:edits {path [e]} :preview {path text}}) or single-file
  ({:path p :edits [e] :preview text})."
  [root opts r]
  (if (or (not (:ok r)) (nil? (:preview r)))
    r
    (let [edits (if (map? (:edits r)) (:edits r) {(:path r) (:edits r)})
          previews (if (map? (:preview r)) (:preview r) {(:path r) (:preview r)})
          olds (into {} (for [p (keys previews)] [p (slurp p)]))
          edits (into {} (for [[p new] previews]
                           [p (or (not-empty (get edits p))
                                  (let [old (get olds p)]
                                    (if (= old new) [] [(whole-edit old new)])))]))
          changed (into {} (filter (fn [[p new]] (not= new (get olds p)))) previews)
          broken (some (fn [[p new]]
                         (when-let [d (broken-by (get olds p) new)]
                           {:file p :problem (:reason d) :note (syntax/explain d)
                            :line (:line d) :col (:col d)}))
                       changed)
          extra (dissoc r :ok :edits :preview :path :changed)]
      (cond
        broken
        (merge extra {:ok false :reason "the edit would leave the file unreadable; nothing written"}
               broken)

        :else
        (let [apply? (and (:apply opts) (seq changed))]
          (when apply?
            (doseq [[p text] changed]
              (spit p text)
              (refresh-file! root p)))
          (merge extra
                 {:ok true
                  :changed (boolean (seq changed))
                  :applied (boolean apply?)
                  :files (vec (sort (keys changed)))
                  :diff (into {} (for [[p _] changed]
                                   [p (diff/diff (get olds p) (get edits p))]))
                  :edits (into {} (for [[p es] edits :when (contains? changed p)]
                                    [p (mapv edit-view es)]))}))))))

(defn rename* [root opts]
  (let [d (ensure-db root)
        opts (-> opts
                 (update :path #(resolve-path root (or % (:file opts))))
                 (update :row #(or % (:line opts))))]
    (finish root opts (rename/rename d opts))))

(defn clean-ns* [root opts]
  (let [d (ensure-db root)
        path (resolve-path root (:file opts))]
    (finish root opts (or (clean-ns/plan d path)
                          {:ok false :reason (str "not an indexed file: " path)}))))

(defn add-require* [root opts]
  (let [d (ensure-db root)
        path (resolve-path root (or (:file opts) (:path opts)))]
    (finish root opts (add-require/plan d (assoc opts :path path)))))

(defn- vet-new [text]
  (when-not (string/blank? text)
    (when-let [d (syntax/diagnose text)]
      {:ok false :reason "new text does not read as Clojure"
       :problem (:reason d) :note (syntax/explain d)})))

(defn replace-form*
  "Replace the form(s) that read equal to :old-form (anywhere in the file,
  ignoring whitespace and formatting), or the text in :range."
  [root opts]
  (let [path (resolve-path root (:file opts))
        source (slurp path)
        new (or (:new-text opts) "")]
    (or (vet-new new)
        (cond
          (:old-form opts)
          (let [r (replace/replace-form source (:old-form opts) new
                                        {:occurrence (:occurrence opts) :all? (:all opts)})]
            (if-let [e (:error r)]
              (cond-> {:ok false :reason (name e) :message (:message r)}
                (:diagnosis r) (assoc :note (syntax/explain (:diagnosis r)))
                (seq (:matches r)) (assoc :matches (mapv #(select-keys % [:line :col :text])
                                                         (:matches r))))
              (finish root opts {:ok true :path path :preview (:content r)
                                 :matches (mapv #(select-keys % [:line :col]) (:matches r))})))

          (:range opts)
          (let [e (vedit/range->edit source (:range opts) new)]
            (finish root opts {:ok true :path path :edits [e]
                               :preview (vedit/apply-edits source [e])}))

          :else {:ok false :reason "pass old_form or range"}))))

(defn- indent-lines [text col]
  (let [pad (apply str (repeat (dec col) " "))
        [first-line & more] (string/split text #"\n" -1)]
    (string/join "\n" (cons first-line (map #(if (string/blank? %) % (str pad %)) more)))))

(defn insert-form*
  "Insert :new-text after (or, with :before, before) the form that reads
  equal to :after/:before, or append it to the file when no anchor is
  given. The text is indented to the anchor's column."
  [root opts]
  (let [path (resolve-path root (:file opts))
        source (slurp path)
        new (string/trim (or (:new-text opts) ""))
        anchor (or (:after opts) (:before opts))
        before? (and (:before opts) (not (:after opts)))]
    (or (vet-new new)
        (if (nil? anchor)
          (let [sep (if (string/ends-with? source "\n") "\n" "\n\n")
                e (vedit/offsets->edit source (count source) (count source) (str sep new "\n"))]
            (finish root opts {:ok true :path path :edits [e]
                               :preview (vedit/apply-edits source [e])}))
          (let [ms (replace/find-forms source anchor)
                m (cond
                    (:occurrence opts) (nth ms (dec (:occurrence opts)) nil)
                    (= 1 (count ms)) (first ms))]
            (cond
              (empty? ms) {:ok false :reason "anchor form not found"}
              (nil? m) {:ok false :reason (str (count ms) " forms match the anchor; pass occurrence")
                        :matches (mapv #(select-keys % [:line :col :text]) ms)}
              :else
              (let [col (:col m)
                    sep (if (= 1 col) "\n\n" (str "\n" (apply str (repeat (dec col) " "))))
                    text (indent-lines new col)
                    e (if before?
                        (vedit/offsets->edit source (:start m) (:start m) (str text sep))
                        (vedit/offsets->edit source (:end m) (:end m) (str sep text)))]
                (finish root opts {:ok true :path path :edits [e]
                                   :preview (vedit/apply-edits source [e])}))))))))

(defn refactor* [root opts]
  (let [d (ensure-db root)
        file (resolve-path root (:file opts))
        target (some->> (:target-file opts) (resolve-path root))
        r (case (keyword (:kind opts))
            :sort-requires (refactor/sort-requires d file)
            :cycle-keyword (refactor/cycle-keyword d file (:line opts) (:col opts))
            :inline-symbol (refactor/inline-symbol d file (:line opts) (:col opts))
            :destructure-keys (refactor/destructure-keys d file (:line opts) (:col opts))
            :restructure-keys (refactor/restructure-keys d file (:line opts) (:col opts))
            :move-form (refactor/move-form d file (:line opts) (:col opts) target)
            {:ok false :reason (str "unknown refactor " (:kind opts))})]
    (finish root opts r)))

(defn format* [root opts]
  (let [path (resolve-path root (:file opts))]
    (finish root opts (fmt/format-file (root-for root) path))))

;; ---------------------------------------------------------------------------
;; structural diff

(defn- relative [root path]
  (str (fs/relativize (fs/path (root-for root)) (fs/path path))))

(defn structural-diff
  "What changed in a Clojure file, by form and by where in the form: renames,
  extractions, moves, wrapping, comment-only and whitespace-only edits told
  apart from changes in behaviour. The old side is `:old` (text), `:old-file`,
  or the file at git revision `:rev` (default HEAD); the new side is `:new`
  (text) or the file on disk."
  [root opts]
  (let [file (resolve-path root (:file opts))
        new (if (contains? opts :new) (str (:new opts)) (if (fs/exists? file) (slurp file) ""))
        old (cond (contains? opts :old) (str (:old opts))
                  (:old-file opts) (slurp (resolve-path root (:old-file opts)))
                  :else (git/show (root-for root) (or (:rev opts) "HEAD") file))]
    (if (nil? old)
      {:ok false :reason "no old text: pass old or old-file, or work inside a git repository"}
      (let [{:keys [files renames]} (sdiff/rollup-renames [(sdiff/file-report (relative root file) old new)])
            report {:clj files :renames renames}]
        {:ok true
         :file (relative root file)
         :verdict (:verdict (first files))
         :renames renames
         :groups (select-keys (sdiff-group/by-calls report) [:groups :shared :singles])
         :text (string/trim (sdiff-text/report-text report))
         :forms (:forms (sdiff-edn/file->edn (first files)))}))))

;; ---------------------------------------------------------------------------
;; project restructuring: plans over the project's sources, applied whole

(defn- project-sources
  "{root-relative path text} of the project's own Clojure files."
  [root d]
  (into {} (for [f (db/all-files d)
                 :when (and (not (:external f)) (#{:clj :cljc} (:dialect f)))]
             [(relative root (:path f)) (slurp (:path f))])))

(defn- finish-plan
  "Preview, check and optionally apply a plan's whole-file results.
  `futures` is {relative path text}, `deleted` relative paths to remove.
  `:texts true` in opts adds :texts, the new text of every changed file, for
  a library caller that publishes the change itself."
  [root opts futures deleted extra]
  (let [abs #(resolve-path root %)
        old-of (fn [p] (let [f (abs p)] (if (fs/exists? f) (slurp f) "")))
        changed (into {} (remove (fn [[p text]] (= text (old-of p)))) futures)
        broken (some (fn [[p text]]
                       (when-let [d (if (fs/exists? (abs p)) (broken-by (old-of p) text) (syntax/diagnose text))]
                         {:file p :problem (:reason d) :note (syntax/explain d)
                          :line (:line d) :col (:col d)}))
                     changed)]
    (if broken
      (merge extra {:ok false :reason "the change would leave a file unreadable; nothing written"} broken)
      (let [apply? (and (:apply opts) (or (seq changed) (seq deleted)))]
        (when apply?
          (doseq [[p text] changed]
            (fs/create-dirs (fs/parent (abs p)))
            (spit (abs p) text))
          (doseq [p deleted] (fs/delete-if-exists (abs p)))
          (refresh! root))
        (cond-> (merge extra
               {:ok true
                :changed (boolean (or (seq changed) (seq deleted)))
                :applied (boolean apply?)
                :files (vec (sort (keys changed)))
                :created (vec (sort (remove #(fs/exists? (abs %)) (keys changed))))
                :deleted (vec (sort deleted))
                :diff (into {} (for [[p text] changed
                                     :let [old (old-of p)]]
                                 [p (diff/diff old [(whole-edit old text)])]))})
          (:texts opts) (assoc :texts changed))))))

(defn- plan-error [plan]
  (when-let [t (or (:error-type plan) (:error plan))]
    (merge {:ok false :reason (str (name t))}
           (dissoc plan :error-type :error))))

(defn- caller-decisions
  "Which files that mention a moved var are its callers, from the index: a
  file with a reference to one of `fqns` is :review-required (it still names
  the old place), any other file :not-a-caller."
  [d root fqns]
  (let [callers (set (for [fqn fqns r (db/refs-by-fqn d fqn)
                           :let [f (db/file-by-id d (:file-id r))]
                           :when f]
                       (relative root (:path f))))]
    (fn [files] (into {} (for [f files] [f (if (callers f) :review-required :not-a-caller)])))))

(defn rename-namespace*
  "Rename namespace `:from` to `:to`: its file moves to the path the new name
  implies, and every ns form, require and qualified reference in the project
  follows."
  [root opts]
  (let [d (ensure-db root)
        sources (project-sources root d)
        base {:sources sources :from (symbol (str (:from opts))) :to (symbol (str (:to opts)))}
        first-try (ns-rename/compile-plan base)
        plan (if (= :caller-disposition-required (:error-type first-try))
               (ns-rename/compile-plan
                (assoc base :caller-decisions
                       (into {} (for [f (get-in first-try [:caller-proof :unknown-callers])]
                                  [f :review-required]))))
               first-try)]
    (or (plan-error plan)
        (finish-plan root opts (:future-sources plan) (:deleted-files plan)
                     {:moves (:file-moves plan)}))))

(defn fix-declares*
  "Remove `declare`s a file no longer needs by moving each definition above
  its first use; declares that break a real cycle stay."
  [root opts]
  (let [file (resolve-path root (:file opts))
        rel (relative root file)
        plan (fix-declares/plan-fix (slurp file) rel)]
    (or (plan-error plan)
        (finish-plan root opts {rel (:future-source plan)} []
                     {:summary (:summary plan) :needed-declares (:needed-declares plan)}))))

(defn extract*
  "Move the named top-level `:forms` of `:file`, with the private helpers only
  they use, into namespace `:to-ns` (created at the path it implies). Files
  the index shows still calling a moved var are listed as callers to update."
  [root opts]
  (let [d (ensure-db root)
        file (resolve-path root (:file opts))
        rel (relative root file)
        src-ns (:ns (db/file-by-path d file))
        to-ns (str (:to-ns opts))
        to (str (or (some-> (fs/parent rel) str (string/split #"/") first) "src") "/"
                (-> to-ns (string/replace "-" "_") (string/replace "." "/")) ".clj")
        names (mapv symbol (:forms opts))
        sources (assoc (project-sources root d) to (get (project-sources root d) to))
        decide (caller-decisions d root (map #(str src-ns "/" %) names))
        base {:file rel :sources sources :forms names :to to :target-ns to-ns
              :derive-required-public-forms true}
        first-try (extract/compile-plan base)
        plan (if (= :caller-disposition-required (:error-type first-try))
               (extract/compile-plan
                (assoc base :caller-decisions
                       (decide (get-in first-try [:caller-proof :unknown-callers]))))
               first-try)]
    (or (plan-error plan)
        (finish-plan root opts (:future-sources plan) []
                     {:target to
                      :callers (vec (sort (keep (fn [[f v]] (when (= :review-required v) f))
                                                (get-in plan [:caller-proof :dispositions]))))}))))

