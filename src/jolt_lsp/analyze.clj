(ns jolt-lsp.analyze
  "Source text -> facts.

  The reader is external (`jolt-lsp.read-impl`); everything here walks the
  node tree it returns. The walker tracks scopes so a local reference is
  recorded as a local, never as a var, and records var, keyword, and call
  occurrences with positions."
  (:require [clojure.string :as string]
            [jolt-lsp.read-impl :as read]
            [jolt-lsp.tree :as tree]))

;; ---------------------------------------------------------------------------
;; names and sets

(def def-kinds
  {"def" :def
   "defonce" :defonce
   "defn" :defn
   "defn-" :defn
   "defmacro" :defmacro
   "defmulti" :defmulti
   "defprotocol" :defprotocol
   "defrecord" :defrecord
   "deftype" :deftype
   "deftest" :deftest
   "defspec" :def
   "declare" :declare})

(def private-defs #{"defn-"})

(def binding-heads
  #{"let" "let*" "loop" "loop*" "when-let" "if-let" "when-some" "if-some"
    "when-first" "dotimes" "with-open" "with-local-vars" "binding"
    "for" "doseq"})

(def special-heads
  #{"do" "if" "if-not" "when" "when-not" "cond" "condp" "case" "and" "or"
    "->" "->>" "some->" "some->>" "cond->" "cond->>" "doto" "try" "catch"
    "finally" "throw" "new" "set!" "recur" "monitor-enter" "monitor-exit"
    "deftype*" "reify*" "case*" "let*" "fn*" "loop*" "import*" "def*"
    "quarter" "await" "io!" "future" "delay"})

(def core-specials
  #{"def" "if" "do" "let*" "quote" "var" "fn*" "loop*" "recur" "throw"
    "try" "catch" "finally" "new" "set!" "monitor-enter" "monitor-exit"
    "." "deftype*" "reify*" "case*" "import*" "def*" "in-ns" "ns"})

(def skip-symbols
  #{"_" "&" "..." "%" "%1" "%2" "%3" "%&" "unquote" "unquote-splicing"})

(defn- norm-name [x]
  (when x (str x)))

;; ---------------------------------------------------------------------------
;; state

(defn- new-state []
  {:vars (atom [])
   :occurrences (atom [])
   :keywords (atom [])
   :locals (atom [])
   :local-refs (atom [])
   :local-seq (atom 0)})

(defn- sym-text [node]
  (when (and node (tree/symbol-node? node))
    (str (tree/sexpr node))))

(defn- kw-text [node]
  (when (and node (tree/keyword-node? node))
    (tree/text node)))

(declare walk-node expand-libspec walk-quoted)

(defn- add-local! [st node kind scope-range keys-destructuring? destructured?]
  (let [id (swap! (:local-seq st) inc)
        nm (sym-text node)]
    (when (and nm (not (skip-symbols nm)))
      (swap! (:locals st) conj
             {:local-id id
              :name nm
              :kind kind
              :range (tree/node-range node)
              :scope-range scope-range
              :keys-destructuring keys-destructuring?
              :destructured destructured?
              :text (tree/text node)})
      id)))

(defn- add-local-ref! [st local-id node]
  (swap! (:local-refs st) conj
         {:local-id local-id
          :name (sym-text node)
          :range (tree/node-range node)
          :text (tree/text node)}))

(defn- add-occ! [ctx st node in-call?]
  (let [nm (sym-text node)]
    (when (and nm (not (skip-symbols nm)))
      (if-let [local-id (get (:locals ctx) nm)]
        (add-local-ref! st local-id node)
        (let [slash (string/index-of nm "/")
              qualified? (and slash (> slash 0) (< slash (dec (count nm))))
              prefix (when qualified? (subs nm 0 slash))
              name (if qualified? (subs nm (inc slash)) nm)
              r (tree/node-range node)
              name-col (first r)
              name-range (if qualified?
                           [(first r)
                            (+ (second r) slash 1)
                            (nth r 2)
                            (nth r 3)]
                           r)]
          (swap! (:occurrences st) conj
                 {:text nm
                  :name name
                  :prefix prefix
                  :qualified qualified?
                  :in-call in-call?
                  :caller (:current-var ctx)
                  :ns (:ns ctx)
                  :range r
                  :name-range name-range}))))))

(defn- add-keyword! [ctx st node def?]
  (let [text (kw-text node)]
    (when text
      (let [auto? (string/starts-with? text "::")
            body (if auto? (subs text 2) (subs text 1))
            aslash (string/index-of body "/")
            prefix (when aslash (subs body 0 aslash))
            name (if aslash (subs body (inc aslash)) body)
            alias? (and auto? (some? aslash))
            target (cond
                     alias? (get (:aliases ctx) prefix)
                     auto? (:ns ctx)
                     aslash prefix
                     :else nil)
            fqn (cond
                  (and auto? alias?) (when target (str ":" target "/" name))
                  auto? (when (:ns ctx) (str ":" (:ns ctx) "/" name))
                  aslash (str ":" prefix "/" name)
                  :else (str ":" name))
            notation (cond auto? (if alias? :auto-alias :auto)
                            aslash :qualified
                            :else :plain)]
        (when fqn
          (swap! (:keywords st) conj
                 {:text text
                  :fqn fqn
                  :notation notation
                  :alias prefix
                  :name name
                  :def? (boolean def?)
                  :range (tree/node-range node)
                  :ns (:ns ctx)}))))))

;; ---------------------------------------------------------------------------
;; definitions

(defn- doc-string [nodes]
  (some (fn [n] (when (tree/string-node? n) (tree/sexpr n))) nodes))

(defn- attr-private? [nodes]
  (or (some (fn [n]
              (and (= :token (tree/tag n))
                   (string/includes? (tree/text n) "private")))
            nodes)
      (some (fn [n]
              (and (= :map (tree/tag n))
                   (string/includes? (tree/text n) ":private")))
            nodes)))

(defn- params-node [nodes]
  (some (fn [n] (when (= :vector (tree/tag n)) n)) nodes))

(defn- arity-nodes [nodes]
  (vec (filter #(= :list (tree/tag %)) nodes)))

(defn- arglists-str [params arities]
  (->> (cond
         params [(str "(" (tree/text params) ")")]
         (seq arities) (map (fn [a]
                              (let [p (first (tree/children a))]
                                (str "(" (tree/text p) ")")))
                            arities)
         :else [])
       (string/join " ")))

(defn- param-names [params]
  (when params
    (->> (tree/children params)
         (keep sym-text)
         (remove skip-symbols)
         vec)))

(defn- define-var! [st ctx name-node nm kind doc arglists params defined-by private? form-range]
  (let [name-node (tree/meta-form name-node)
        ns (:ns ctx)
        fqn (if ns (str ns "/" nm) nm)]
    (swap! (:vars st) conj
           {:name nm
            :ns ns
            :fqn fqn
            :kind kind
            :private (boolean private?)
            :doc doc
            :arglists arglists
            :params (or params [])
            :defined-by defined-by
            :path (:path ctx)
            :range form-range
            :name-range (tree/node-range (tree/meta-form name-node))})
    fqn))

;; ---------------------------------------------------------------------------
;; binding patterns

(defn- bind-pattern [node ctx st kind scope-range]
  (cond
    (nil? node) ctx
    (tree/symbol-node? node)
    (let [nm (sym-text node)]
      (if (or (skip-symbols nm) (nil? nm))
        ctx
        (let [id (add-local! st node kind scope-range false false)]
          (assoc-in ctx [:locals nm] id))))

    (= :vector (tree/tag node))
    (reduce (fn [ctx child]
              (cond
                (= "&" (sym-text child)) ctx
                (= "&" (tree/text child)) ctx
                (= ":as" (tree/text child)) ctx
                :else (bind-pattern child ctx st kind scope-range)))
            ctx
            (tree/children node))

    (= :map (tree/tag node))
    (reduce (fn [ctx [k v]]
              (let [kt (tree/text k)]
                (cond
                  (and (string/starts-with? kt ":")
                       (contains? #{":keys" ":strs" ":syms" ":keys!" ":strs!" ":syms!"
                                    ":as" ":or" ":rename"}
                                  kt)
                       (#{":keys" ":strs" ":syms" ":keys!" ":strs!" ":syms!"} kt)
                       (= :vector (tree/tag v)))
                  (reduce (fn [ctx entry]
                            (let [en (tree/meta-form entry)
                                  et (tree/text en)
                                  slash (string/index-of et "/")
                                  nm (if slash (subs et (inc slash)) et)]
                              (when (and (string/starts-with? kt ":") (:ns ctx))
                                (let [prefix (when slash (subs et 0 slash))
                                      target (if prefix (get (:aliases ctx) prefix) (:ns ctx))
                                      fqn (when target (str ":" target "/" nm))]
                                  (when fqn
                                    (swap! (:keywords st) conj
                                           {:text (str ":" (if prefix (str prefix "/") "") nm)
                                            :fqn fqn
                                            :notation (if prefix :alias :auto)
                                            :name nm
                                            :def? false
                                            :range (tree/node-range en)
                                            :ns (:ns ctx)}))))
                              (if (or (nil? nm) (skip-symbols nm))
                                ctx
                                (let [id (add-local! st en kind scope-range true true)]
                                  (assoc-in ctx [:locals nm] id)))))
                          ctx
                          (tree/children v))

                  (= ":as" kt)
                  (bind-pattern v ctx st kind scope-range)

                  (= ":or" kt)
                  ctx

                  :else
                  (bind-pattern v ctx st kind scope-range))))
            ctx
            (partition-all 2 (tree/children node)))

    :else ctx))

(defn- bind-let-vector [vnode ctx st kind scope-range]
  (reduce (fn [ctx [pattern init]]
            (when init (walk-node init ctx st))
            (bind-pattern pattern ctx st kind scope-range))
          ctx
          (partition-all 2 (tree/children vnode))))

(defn- bind-for-vector [vnode ctx st kind scope-range]
  (if-not (and vnode (tree/branch? vnode))
    ctx
    (reduce (fn [ctx [a b]]
            (let [at (tree/text a)]
              (cond
                (= at ":let") (bind-let-vector b ctx st :let scope-range)
                (contains? #{":when" ":while"} at) (do (walk-node b ctx st) ctx)
                :else (do (walk-node b ctx st)
                          (bind-pattern a ctx st kind scope-range)))))
          ctx
          (partition-all 2 (tree/children vnode)))))

;; ---------------------------------------------------------------------------
;; walking

(defn- walk-body [nodes ctx st]
  (doseq [n nodes] (walk-node n ctx st)))

(defn- walk-fn-tail [nodes ctx st]
  (let [params (params-node nodes)
        arities (arity-nodes nodes)]
    (cond
      params
      (let [pctx (bind-pattern params ctx st :param (tree/node-range params))]
        (walk-body (drop-while #(not (identical? % params)) nodes) pctx st))

      (seq arities)
      (doseq [arity arities]
        (let [cs (tree/children arity)
              p (first cs)
              pctx (if (= :vector (tree/tag p))
                     (bind-pattern p ctx st :param (tree/node-range arity))
                     ctx)]
          (walk-body (rest (drop-while #(not (identical? % p)) cs)) pctx st)))

      :else (walk-body nodes ctx st))))

(defn- handle-def [node ctx st h]
  (let [cs (tree/children node)
        name-node (second cs)
        nm (some-> name-node tree/meta-form sym-text)
        kind (get def-kinds h)
        after-name (drop 2 cs)
        doc (doc-string after-name)
        params (params-node after-name)
        arities (arity-nodes after-name)
        arglists (arglists-str params arities)
        pnames (param-names params)
        private? (or (private-defs h)
                     (and name-node (string/includes? (tree/text name-node) "private"))
                     (some #(and (= :map (tree/tag %))
                                 (string/includes? (tree/text %) ":private"))
                           after-name))
        ctx' (assoc ctx :current-var (when nm (if (:ns ctx) (str (:ns ctx) "/" nm) nm)))]
    (when nm
      (when-not (= h "declare")
        (define-var! st ctx name-node nm kind doc arglists pnames h (boolean private?) (tree/node-range node)))
      (case h
        ("defn" "defn-" "defmacro" "deftest" "defspec")
        (walk-fn-tail after-name ctx' st)

        "defmethod"
        nil

        "defprotocol"
        (doseq [m (filter #(= :list (tree/tag %)) after-name)]
          (let [mcs (tree/children m)
                mname (first mcs)
                mnm (sym-text mname)
                mdoc (doc-string (rest mcs))
                mparams (params-node (rest mcs))]
            (when mnm
              (swap! (:vars st) conj
                     {:name mnm
                      :ns (:ns ctx)
                      :fqn (if (:ns ctx) (str (:ns ctx) "/" mnm) mnm)
                      :kind :protocol-method
                      :private false
                      :doc mdoc
                      :arglists (arglists-str mparams nil)
                      :params (or (param-names mparams) [])
                      :defined-by "defprotocol"
                      :path (:path ctx)
                      :range (tree/node-range m)
                      :name-range (tree/node-range mname)}))))

        ("defrecord" "deftype")
        (let [fields (some #(when (= :vector (tree/tag %)) %) after-name)
              fctx (if fields
                     (reduce (fn [c f] (bind-pattern f c st :field (tree/node-range node)))
                             ctx
                             (tree/children fields))
                     ctx)]
          (doseq [m (filter #(= :list (tree/tag %)) after-name)]
            (let [mcs (tree/children m)
                  p (some #(when (= :vector (tree/tag %)) %) mcs)]
              (when p
                (let [mctx (bind-pattern p fctx st :param (tree/node-range m))]
                  (walk-body (rest (drop-while #(not (identical? % p)) mcs)) mctx st))))))

        "declare"
        (doseq [n (rest cs)]
          (when (tree/symbol-node? n)
            (let [nm2 (sym-text n)]
              (swap! (:vars st) conj
                     {:name nm2
                      :ns (:ns ctx)
                      :fqn (if (:ns ctx) (str (:ns ctx) "/" nm2) nm2)
                      :kind :declare
                      :private false
                      :doc nil
                      :arglists nil
                      :params []
                      :defined-by "declare"
                      :path (:path ctx)
                      :range (tree/node-range n)
                      :name-range (tree/node-range n)}))))

        ;; def, defonce, defmulti: walk the value/dispatch form
        (doseq [n (drop-while #(or (tree/string-node? %)
                                   (= :map (tree/tag %)))
                              after-name)]
          (when n (walk-node n ctx' st)))))))

(defn- handle-defmethod [node ctx st]
  (let [cs (tree/children node)
        mm (second cs)
        dispatch (nth cs 2 nil)
        after (drop 3 cs)
        mm-text (sym-text mm)]
    (when mm
      (add-occ! ctx st mm true))
    (when dispatch
      (if (tree/keyword-node? dispatch)
        (let [ig? (or (= mm-text "ig/init-key")
                      (= mm-text "integrant.core/init-key")
                      (some-> (get (:aliases ctx) (first (string/split mm-text #"/")))
                              (= "integrant.core")))]
          (add-keyword! ctx st dispatch (boolean ig?)))
        (walk-node dispatch ctx st)))
    (walk-fn-tail after ctx st)))

(defn- handle-defprotocol [node ctx st]
  (handle-def node ctx st "defprotocol"))

(defn- handle-deftype [node ctx st h]
  (handle-def node ctx st h))

(defn- handle-declare [node ctx st]
  (handle-def node ctx st "declare"))

(defn- handle-let [node ctx st h]
  (let [cs (tree/children node)
        v (second cs)
        body (drop 2 cs)
        scope (tree/node-range node)]
    (if-not (and v (tree/branch? v))
      (walk-body body ctx st)
      (cond
        (= h "binding")
        (do
          (doseq [[_ init] (partition-all 2 (tree/children v))]
            (when init (walk-node init ctx st)))
          (walk-body body ctx st))

        (contains? #{"when-let" "if-let" "when-some" "if-some"} h)
        (let [[pat init] (tree/children v)
              _ (when init (walk-node init ctx st))
              ctx' (bind-pattern pat ctx st (keyword h) scope)]
          (walk-body body ctx' st))

        (= h "when-first")
        (let [[pat init] (tree/children v)
              _ (when init (walk-node init ctx st))
              ctx' (bind-pattern pat ctx st :when-first scope)]
          (walk-body body ctx' st))

        (= h "dotimes")
        (let [[pat init] (tree/children v)
              _ (when init (walk-node init ctx st))
              ctx' (bind-pattern pat ctx st :dotimes scope)]
          (walk-body body ctx' st))

        (contains? #{"for" "doseq"} h)
        (let [ctx' (bind-for-vector v ctx st (keyword h) scope)]
          (walk-body body ctx' st))

        :else
        (let [ctx' (bind-let-vector v ctx st (keyword h) scope)]
          (walk-body body ctx' st))))))

(defn- handle-fn [node ctx st]
  (let [cs (tree/children node)
        after (rest cs)
        maybe-name (first after)
        named? (and (tree/symbol-node? maybe-name)
                    (not= :vector (tree/tag (second after))))
        nm (when named? (sym-text maybe-name))
        ctx' (if nm (assoc-in ctx [:locals nm] (add-local! st maybe-name :fn (tree/node-range node) false false)) ctx)]
    (walk-fn-tail (if named? (rest after) after) ctx' st)))

(defn- handle-letfn [node ctx st]
  (let [cs (tree/children node)
        v (second cs)
        body (drop 2 cs)
        specs (if (and v (tree/branch? v)) (tree/children v) [])
        ctx' (reduce (fn [c spec]
                       (let [n (first (tree/children spec))
                             nm (sym-text n)]
                         (if nm
                           (assoc-in c [:locals nm] (add-local! st n :letfn (tree/node-range node) false false))
                           c)))
                     ctx
                     specs)]
    (doseq [spec specs]
      (let [cs2 (tree/children spec)
            p (some #(when (= :vector (tree/tag %)) %) cs2)]
        (when p
          (let [pctx (bind-pattern p ctx' st :param (tree/node-range spec))]
            (walk-body (rest (drop-while #(not (identical? % p)) cs2)) pctx st)))))
    (walk-body body ctx' st)))

(defn- handle-methods [node ctx st methods]
  (doseq [m (filter #(= :list (tree/tag %)) methods)]
    (let [cs (tree/children m)
          p (some #(when (= :vector (tree/tag %)) %) cs)]
      (when p
        (let [mctx (bind-pattern p ctx st :param (tree/node-range m))]
          (walk-body (rest (drop-while #(not (identical? % p)) cs)) mctx st))))))

(defn- handle-extend-type [node ctx st]
  (let [cs (tree/children node)]
    (when (second cs) (walk-node (second cs) ctx st))
    (handle-methods node ctx st (drop 2 cs))))

(defn- handle-extend-protocol [node ctx st]
  (let [cs (tree/children node)]
    (when (second cs) (walk-node (second cs) ctx st))
    (doseq [n (drop 2 cs)]
      (when (= :list (tree/tag n))
        (handle-methods node ctx st [n])))))

(defn- handle-as-> [node ctx st]
  (let [cs (tree/children node)
        expr (second cs)
        nm-node (nth cs 2 nil)
        nm (sym-text nm-node)
        _ (when expr (walk-node expr ctx st))
        ctx' (if nm
               (assoc-in ctx [:locals nm] (add-local! st nm-node :as-> (tree/node-range node) false false))
               ctx)]
    (walk-body (drop 3 cs) ctx' st)))

(defn- handle-ns [node ctx st]
  (let [cs (tree/children node)
        nm-node (second cs)
        nm (some-> nm-node tree/meta-form sym-text)
        clauses (drop 2 cs)]
    (if nm
      (let [st-meta (atom {:aliases {} :refers {} :requires [] :require-specs []
                           :refer-sites [] :imports {} :refer-all [] :as-aliases []
                           :core-excludes []})]
        (doseq [clause clauses]
          (when (= :list (tree/tag clause))
            (let [ccs (tree/children clause)
                  kw (tree/text (first ccs))
                  items (rest ccs)]
              (case kw
                ":require"
                (doseq [lib items
                        spec (expand-libspec lib)]
                  (swap! st-meta update :requires conj (:target spec))
                  (swap! st-meta update :require-specs conj spec)
                  (when (:alias spec)
                    (swap! st-meta assoc-in [:aliases (:alias spec)] (:target spec)))
                  (when (:refer-all spec)
                    (swap! st-meta update :refer-all conj (:target spec)))
                  (when (:as-alias spec)
                    (swap! st-meta update :as-aliases conj (:target spec)))
                  (doseq [[local target] (:refers spec)]
                    (swap! st-meta assoc-in [:refers local] target))
                  (swap! st-meta update :refer-sites into (:refer-sites spec)))

                ":use"
                (doseq [lib items
                        spec (expand-libspec lib)]
                  (swap! st-meta update :requires conj (:target spec))
                  (swap! st-meta update :require-specs conj spec)
                  (swap! st-meta update :refer-all conj (:target spec)))

                ":import"
                (doseq [lib items]
                  (let [txt (tree/text lib)
                        parts (string/split (string/replace txt #"[()\[\]]" "") #"\s+")
                        fq (last parts)
                        simple (last (string/split fq #"\."))]
                    (when (and fq simple)
                      (swap! st-meta assoc-in [:imports simple] fq))))

                ":refer-clojure"
                (doseq [[k v] (partition-all 2 items)]
                  (when (= ":exclude" (tree/text k))
                    (doseq [n (tree/children v)]
                      (when-let [nm2 (sym-text n)]
                        (swap! st-meta update :core-excludes conj nm2)))))

                nil))))
        (assoc ctx
               :ns nm
               :ns-meta @st-meta
               :aliases (:aliases @st-meta)
               :refers (:refers @st-meta)
               :core-excludes (:core-excludes @st-meta)))
      ctx)))

(defn- handle-in-ns [node ctx st]
  (let [cs (tree/children node)
        arg (second cs)
        sym (cond
              (tree/quote? arg) (some-> (first (tree/children arg)) tree/sexpr str)
              (tree/symbol-node? arg) (sym-text arg)
              :else nil)]
    (if sym (assoc ctx :ns sym) ctx)))

(defn- walk-call [node ctx st]
  (let [cs (tree/children node)]
    (when (seq cs)
      (let [head (first cs)]
        (if (tree/symbol-node? head)
          (add-occ! ctx st head true)
          (walk-node head ctx st))
        (doseq [n (rest cs)] (walk-node n ctx st))))))

(defn- walk-list [node ctx st]
  (let [h (some-> node (tree/children) first sym-text)]
    (cond
      (= h "ns") (handle-ns node ctx st)
      (= h "in-ns") (handle-in-ns node ctx st)
      (= h "comment") nil
      (= h "quote") (when-let [n (second (tree/children node))]
                      (walk-quoted n ctx st))
      (= h "defmethod") (handle-defmethod node ctx st)
      (= h "defprotocol") (handle-defprotocol node ctx st)
      (= h "defrecord") (handle-deftype node ctx st "defrecord")
      (= h "deftype") (handle-deftype node ctx st "deftype")
      (= h "declare") (handle-declare node ctx st)
      (= h "fn") (handle-fn node ctx st)
      (= h "fn*") (handle-fn node ctx st)
      (= h "letfn") (handle-letfn node ctx st)
      (= h "reify") (handle-methods node ctx st (rest (tree/children node)))
      (= h "extend-type") (handle-extend-type node ctx st)
      (= h "extend-protocol") (handle-extend-protocol node ctx st)
      (= h "as->") (handle-as-> node ctx st)
      (and h (contains? binding-heads h)) (handle-let node ctx st h)
      (and h (contains? def-kinds h)) (handle-def node ctx st h)
      :else (walk-call node ctx st))))

(def collection-tags
  #{:forms :list :vector :map :set :fn :quote :syntax-quote :unquote
    :unquote-splicing :deref :var :meta :reader-macro :namespaced-map})

(defn- walk-quoted [node ctx st]
  (when node
    (cond
      (tree/keyword-node? node) (add-keyword! ctx st node false)
      (tree/uneval? node) nil
      (contains? collection-tags (tree/tag node))
      (doseq [c (tree/children node)] (walk-quoted c ctx st))
      :else nil)))

(defn- walk-node [node ctx st]
  (when node
    (let [tag (tree/tag node)]
      (cond
        (tree/uneval? node) nil
        (tree/keyword-node? node) (add-keyword! ctx st node false)
        (= :token tag) (add-occ! ctx st node false)
        (= :quote tag) (doseq [c (tree/children node)] (walk-quoted c ctx st))
        (= :syntax-quote tag) (doseq [c (tree/children node)] (walk-node c ctx st))
        (contains? #{:unquote :unquote-splicing :deref :var} tag)
        (doseq [c (tree/children node)] (walk-node c ctx st))
        (= :meta tag) (walk-node (tree/meta-form node) ctx st)
        (= :list tag) (walk-list node ctx st)
        (= :reader-macro tag)
        (let [txt (tree/text node)]
          (cond
            (string/starts-with? txt "#_") nil
            (string/starts-with? txt "#?") nil
            :else (doseq [c (tree/children node)] (walk-node c ctx st))))
        (= :namespaced-map tag)
        (doseq [c (tree/children node)] (walk-node c ctx st))
        (contains? #{:vector :map :set} tag)
        (doseq [c (tree/children node)] (walk-node c ctx st))
        (= :fn tag)
        (doseq [c (tree/children node)] (walk-node c ctx st))
        (contains? collection-tags tag)
        (doseq [c (tree/children node)] (walk-node c ctx st))
        :else nil))))

;; ---------------------------------------------------------------------------
;; libspecs

(defn- libspec-target [node]
  (let [n (tree/meta-form node)]
    (when (tree/symbol-node? n) (sym-text n))))

(defn- expand-libspec [node]
  (let [n (tree/meta-form node)]
    (cond
      (tree/symbol-node? n)
      [{:target (sym-text n)}]

      (= :vector (tree/tag n))
      (let [cs (tree/children n)
            target (libspec-target (first cs))
            opts (rest cs)]
        (when target
          (reduce (fn [specs [k v]]
                    (let [kt (tree/text k)]
                      (case kt
                        ":as" (map #(assoc % :alias (sym-text (tree/meta-form v))) specs)
                        ":as-alias" (map #(assoc % :alias (sym-text (tree/meta-form v))
                                                  :as-alias true) specs)
                        ":refer" (if (= ":all" (tree/text v))
                                   (map #(assoc % :refer-all true) specs)
                                   (let [entries (vec (keep (fn [x]
                                                              (let [xm (tree/meta-form x)
                                                                    nm (sym-text xm)]
                                                                (when nm
                                                                  {:name nm
                                                                   :target (str target "/" nm)
                                                                   :range (tree/node-range xm)})))
                                                            (tree/children v)))
                                         names (into {} (map (juxt :name :target) entries))]
                                     (map #(assoc % :refers names :refer-sites entries) specs)))
                        ":rename" (let [pairs (partition-all 2 (tree/children v))
                                        renames (keep (fn [[a b]]
                                                        (let [an (sym-text (tree/meta-form a))
                                                              bn (sym-text (tree/meta-form b))]
                                                          (when (and an bn) [bn (str target "/" an)])))
                                                      pairs)]
                                    (map #(assoc % :refers (into {} renames)) specs))
                        specs)))
                  [{:target target :range (tree/node-range n)}]
                  (partition-all 2 opts))
          ))

      (and (contains? #{:list :vector} (tree/tag n))
           (seq (tree/children n)))
      (let [cs (tree/children n)
            prefix (libspec-target (first cs))]
        (mapcat (fn [child]
                  (let [sub (expand-libspec child)]
                    (map (fn [s]
                           (if (and prefix (:target s) (not (string/includes? (:target s) ".")))
                             (update s :target #(str prefix "." %))
                             s))
                         sub)))
                (rest cs)))

      :else [])))

;; ---------------------------------------------------------------------------
;; entry

(defn- walk-top [node ctx st]
  (let [n (tree/meta-form node)]
    (cond
      (tree/uneval? n) ctx
      (= :list (tree/tag n)) (or (walk-list n ctx st) ctx)
      :else (do (walk-node n ctx st) ctx))))

(defn analyze
  "Parse source and return the analysis map."
  [source path]
  (let [parsed (read/parse-text source)
        st (new-state)
        ctx {:ns nil
             :ns-meta {:aliases {} :refers {} :requires [] :require-specs []
                       :refer-sites [] :imports {} :refer-all [] :as-aliases []
                       :core-excludes []}
             :aliases {}
             :refers {}
             :core-excludes []
             :current-var nil
             :locals {}
             :path path}
        ctx (reduce (fn [c form] (walk-top form c st)) ctx (:forms parsed))]
    {:path path
     :ns (:ns ctx)
     :ns-meta (:ns-meta ctx)
     :vars @(:vars st)
     :occurrences @(:occurrences st)
     :keywords @(:keywords st)
     :locals @(:locals st)
     :local-refs @(:local-refs st)
     :parse-ok (:ok parsed)
     :parse-error (:error parsed)}))

(defn analyze-edn
  "EDN files contribute keyword occurrences only."
  [source path]
  (let [parsed (read/parse-edn source)
        st (new-state)
        ctx {:ns nil :ns-meta {} :aliases {} :refers {} :path path}]
    (doseq [form (:forms parsed)] (walk-quoted form ctx st))
    {:path path
     :ns nil
     :ns-meta {:aliases {} :refers {} :requires [] :require-specs []
               :refer-sites [] :imports {} :refer-all [] :as-aliases []
               :core-excludes []}
     :vars []
     :occurrences []
     :keywords @(:keywords st)
     :locals []
     :local-refs []
     :parse-ok (:ok parsed)
     :parse-error (:error parsed)}))
