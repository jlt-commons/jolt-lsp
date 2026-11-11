(ns jolt-lsp.analyze
  "Source text -> facts.

  The reader is external (`jolt-lsp.read-impl`); everything here walks the
  node tree it returns. The walker tracks scopes so a local reference is
  recorded as a local, never as a var, and records var, keyword, and call
  occurrences with positions.

  Occurrences carry flags the diagnostics read:
    :interop  Java-style interop or a class name (Math/abs, .method, Foo.)
    :quoted   inside a syntax-quote, resolved when the macro expands
    :inactive inside a reader-conditional branch Jolt does not read"
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

(def skip-symbols
  #{"_" "&" "..." "%" "%1" "%2" "%3" "%4" "%5" "%6" "%7" "%8" "%9" "%&"
    "unquote" "unquote-splicing" "&form" "&env"})

(def ^:dynamic *lint-as*
  "{\"ns/macro\" \"defn\"}: analyze calls to a macro as the built-in form
  named (kondo's :lint-as). Bound by the indexer from the project config."
  {})

(def active-features
  "Reader-conditional features Jolt reads, in the order it prefers them."
  [":jolt" ":clj" ":default"])

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

(defn class-like?
  "Whether a name reads as a Java-style class: its last dotted segment starts
  upper-case (String, java.util.Date)."
  [nm]
  (let [seg (last (string/split (str nm) #"\."))]
    (boolean (and (seq seg) (Character/isUpperCase (first seg))))))

(defn- interop-name?
  "`.method`, `Ctor.`, `Class/static` and bare class names."
  [nm prefix qualified?]
  (or (and (string/starts-with? nm ".") (> (count nm) 1) (not= nm ".."))
      (and (string/ends-with? nm ".") (> (count nm) 1) (not= nm ".."))
      (and qualified? (class-like? prefix))
      (and (not qualified?) (class-like? nm))))

(declare walk-node walk-list expand-libspec walk-quoted analyze*)

(defn- add-local!
  [st node kind scope-range keys-destructuring? destructured? & [nm]]
  (let [id (swap! (:local-seq st) inc)
        nm (or nm (sym-text node))]
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

(defn- bind-local [ctx nm id]
  (if id (assoc-in ctx [:locals nm] id) ctx))

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
              qualified? (boolean (and slash (> slash 0) (< slash (dec (count nm)))))
              prefix (when qualified? (subs nm 0 slash))
              name (if qualified? (subs nm (inc slash)) nm)
              r (tree/node-range node)
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
                  :interop (interop-name? nm prefix qualified?)
                  :quoted (boolean (:syntax-quote ctx))
                  :inactive (boolean (:inactive ctx))
                  :heads (:heads ctx)
                  :range r
                  :name-range name-range}))))))

(defn- add-var!
  "Record a definition, unless it sits in a (comment ...) block."
  [ctx st v]
  (when-not (:in-comment ctx)
    (swap! (:vars st) conj v)))

(defn- record-keyword! [ctx st node text fqn notation alias name def?]
  (when fqn
    (swap! (:keywords st) conj
           {:text text
            :fqn fqn
            :notation notation
            :alias alias
            :name name
            :def? (boolean def?)
            :range (tree/node-range node)
            :ns (:ns ctx)})))

(defn- add-keyword! [ctx st node def?]
  (let [text (kw-text node)]
    (when text
      (let [auto? (string/starts-with? text "::")
            body (if auto? (subs text 2) (subs text 1))
            aslash (string/index-of body "/")
            prefix (when aslash (subs body 0 aslash))
            name (if aslash (subs body (inc aslash)) body)
            alias? (and auto? (some? aslash))
            target (when alias? (get (:aliases ctx) prefix))
            fqn (cond
                  alias? (when target (str ":" target "/" name))
                  auto? (when (:ns ctx) (str ":" (:ns ctx) "/" name))
                  aslash (str ":" prefix "/" name)
                  :else (str ":" name))
            notation (cond auto? (if alias? :auto-alias :auto)
                           aslash :qualified
                           :else :plain)]
        (record-keyword! ctx st node text fqn notation prefix name def?)))))

;; ---------------------------------------------------------------------------
;; definitions

(defn- meta-private?
  "^:private name, ^{:private true} name, through any stack of metadata."
  [node]
  (loop [n node]
    (if (and n (= :meta (tree/tag n)))
      (let [[m inner] (tree/children n)]
        (if (or (= ":private" (tree/text m))
                (and (= :map (tree/tag m))
                     (some (fn [[k v]] (and (= ":private" (tree/text k))
                                            (= "true" (some-> v tree/text))))
                           (partition-all 2 (tree/children m)))))
          true
          (recur inner)))
      false)))

(def ^:private attr-map-defs
  "The definers that take an attr-map after the name and docstring."
  #{"defn" "defn-" "defmacro" "defmulti"})

(defn- attr-map
  "The attr-map leading nodes, the forms after a definition's docstring, or
  nil. Only definers that take one have it, and a map with nothing after it
  is a def's value, not an attr-map."
  [h nodes]
  (let [n (first nodes)]
    (when (and (attr-map-defs h) n (= :map (tree/tag n)) (next nodes))
      n)))

(defn- attr-map-private? [h nodes]
  (boolean
   (when-let [m (attr-map h nodes)]
     (some (fn [[k v]] (and (= ":private" (tree/text k))
                            (= "true" (some-> v tree/text))))
           (partition-all 2 (tree/children m))))))

(defn- doc-string
  "The docstring: a string right after the name with something after it."
  [nodes]
  (let [f (first nodes)]
    (when (and f (tree/string-node? f) (next nodes))
      (tree/sexpr f))))

(defn- fn-tail
  "Split what follows a fn name into its single params vector and body, or
  its arities. Return-type metadata on the params (^String [x]) is allowed."
  [nodes]
  (let [nodes (vec nodes)
        idx (first (keep-indexed (fn [i n] (when (= :vector (tree/tag (tree/meta-form n))) i))
                                 nodes))]
    (if idx
      {:params (tree/meta-form (nth nodes idx))
       :body (subvec nodes (inc idx))}
      {:arities (filterv #(= :list (tree/tag %)) nodes)})))

(defn- arity-parts [arity]
  (let [cs (tree/children arity)
        p (first cs)]
    (if (and p (= :vector (tree/tag (tree/meta-form p))))
      {:params (tree/meta-form p) :body (rest cs)}
      {:params nil :body cs})))

(defn- arglists-str [{:keys [params arities]}]
  (->> (cond
         params [(str "(" (tree/text params) ")")]
         (seq arities) (keep (fn [a]
                               (when-let [p (:params (arity-parts a))]
                                 (str "(" (tree/text p) ")")))
                             arities)
         :else [])
       (string/join " ")))

(defn- param-names [params]
  (when params
    (->> (tree/children params)
         (map tree/meta-form)
         (keep sym-text)
         (remove skip-symbols)
         vec)))

(defn- var-entry [ctx nm kind doc arglists params defined-by private? form-range name-range]
  {:name nm
   :ns (:ns ctx)
   :fqn (if (:ns ctx) (str (:ns ctx) "/" nm) nm)
   :kind kind
   :private (boolean private?)
   :doc doc
   :arglists arglists
   :params (or params [])
   :defined-by defined-by
   :path (:path ctx)
   :range form-range
   :name-range name-range})

;; ---------------------------------------------------------------------------
;; binding patterns

(defn- keys-clause
  "For a map-destructuring key such as :keys, ::keys, :a/keys or :strs,
  {:kind :keys|:strs|:syms :ns ns-or-nil :auto bool}, else nil."
  [ctx kt]
  (when (string/starts-with? kt ":")
    (let [auto? (string/starts-with? kt "::")
          body (if auto? (subs kt 2) (subs kt 1))
          slash (string/index-of body "/")
          prefix (when slash (subs body 0 slash))
          nm (string/replace (if slash (subs body (inc slash)) body) #"!$" "")]
      (when (#{"keys" "strs" "syms"} nm)
        {:kind (keyword nm)
         :auto auto?
         :ns (cond
               (and auto? prefix) (get (:aliases ctx) prefix)
               auto? (:ns ctx)
               :else prefix)}))))

(defn- bind-keys-entry
  "Bind one entry of a :keys/:strs/:syms vector and record the keyword it
  destructures."
  [ctx st clause entry kind scope-range]
  (let [en (tree/meta-form entry)]
    (if (tree/keyword-node? en)
      ;; {:keys [:a ::b]} -- the keyword itself, bound by its name
      (let [t (tree/text en)
            nm (last (string/split (string/replace t #"^::?" "") #"/"))]
        (add-keyword! ctx st en false)
        (bind-local ctx nm (add-local! st en kind scope-range true true nm)))
      (let [et (tree/text en)
            slash (string/index-of et "/")
            prefix (when slash (subs et 0 slash))
            nm (if slash (subs et (inc slash)) et)]
        (when (and (= :keys (:kind clause)) (not (skip-symbols nm)))
          (let [kns (or prefix (:ns clause))]
            (record-keyword! ctx st en (str ":" (when kns (str kns "/")) nm)
                             (str ":" (when kns (str kns "/")) nm)
                             (cond (:auto clause) :auto kns :qualified :else :plain)
                             nil nm false)))
        (if (or (nil? nm) (skip-symbols nm))
          ctx
          (bind-local ctx nm (add-local! st en kind scope-range true true)))))))

(defn- bind-pattern [node ctx st kind scope-range]
  (let [node (some-> node tree/meta-form)]
    (cond
      (nil? node) ctx

      (tree/symbol-node? node)
      (let [nm (sym-text node)]
        (if (or (nil? nm) (skip-symbols nm))
          ctx
          (bind-local ctx nm (add-local! st node kind scope-range false false))))

      (= :vector (tree/tag node))
      (reduce (fn [ctx child]
                (if (#{"&" ":as"} (tree/text child))
                  ctx
                  (bind-pattern child ctx st kind scope-range)))
              ctx
              (tree/children node))

      (= :map (tree/tag node))
      (let [pairs (partition-all 2 (tree/children node))
            ;; :or defaults see the bindings, and may name vars
            ctx' (reduce
                  (fn [ctx [k v]]
                    (let [kt (tree/text k)]
                      (if-let [clause (keys-clause ctx kt)]
                        (if (and v (= :vector (tree/tag v)))
                          (reduce #(bind-keys-entry %1 st clause %2 kind scope-range)
                                  ctx (tree/children v))
                          ctx)
                        (case kt
                          ":as" (bind-pattern v ctx st kind scope-range)
                          ":or" ctx
                          ;; {pattern :key}
                          (do (when v (walk-node v ctx st))
                              (bind-pattern k ctx st kind scope-range))))))
                  ctx
                  pairs)]
        (doseq [[k v] pairs
                :when (and v (= ":or" (tree/text k)) (= :map (tree/tag v)))
                [_ default] (partition-all 2 (tree/children v))]
          (when default (walk-node default ctx' st)))
        ctx')

      :else ctx)))

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

(defn- walk-fn-tail
  "Bind the params of a single-arity or multi-arity fn tail and walk its
  body. `kind` is the local kind for the params."
  ([nodes ctx st] (walk-fn-tail nodes ctx st :param))
  ([nodes ctx st kind]
   (let [{:keys [params body arities]} (fn-tail nodes)]
     (if params
       (walk-body body (bind-pattern params ctx st kind (tree/node-range params)) st)
       (doseq [arity arities]
         (let [{:keys [params body]} (arity-parts arity)
               pctx (if params
                      (bind-pattern params ctx st kind (tree/node-range arity))
                      ctx)]
           (walk-body body pctx st)))))))

(defn- handle-def [node ctx st h]
  (let [cs (tree/children node)
        name-node (second cs)
        nm (some-> name-node tree/meta-form sym-text)
        kind (get def-kinds h)
        after-name (drop 2 cs)
        doc (when-not (#{"deftest" "defspec" "declare" "defrecord" "deftype"} h)
              (doc-string after-name))
        after-doc (if doc (rest after-name) after-name)
        tail (when (#{"defn" "defn-" "defmacro" "defrecord" "deftype"} h)
               (fn-tail after-doc))
        private? (or (private-defs h)
                     (meta-private? name-node)
                     (attr-map-private? h after-doc))
        fqn (when nm (if (:ns ctx) (str (:ns ctx) "/" nm) nm))
        ctx' (assoc ctx :current-var fqn)]
    (when nm
      (when-not (= h "declare")
        (add-var! ctx st
               (var-entry ctx nm kind doc
                          (some-> tail arglists-str)
                          (param-names (:params tail))
                          h private? (tree/node-range node)
                          (tree/node-range (tree/meta-form name-node)))))
      (case h
        ("defn" "defn-" "defmacro")
        (walk-fn-tail (drop-while #(= :map (tree/tag %)) after-doc) ctx' st)

        ("deftest" "defspec")
        (walk-body after-name ctx' st)

        "defprotocol"
        (doseq [m (filter #(= :list (tree/tag %)) after-name)]
          (let [mcs (tree/children m)
                mname (first mcs)
                mnm (sym-text mname)
                vecs (filter #(= :vector (tree/tag %)) (rest mcs))
                mdoc (some #(when (tree/string-node? %) (tree/sexpr %)) (rest mcs))]
            (when mnm
              (add-var! ctx st
                     (var-entry ctx mnm :protocol-method mdoc
                                (string/join " " (map #(str "(" (tree/text %) ")") vecs))
                                (param-names (first vecs))
                                "defprotocol" false (tree/node-range m)
                                (tree/node-range mname))))))

        ("defrecord" "deftype")
        (let [fields (some #(when (= :vector (tree/tag %)) %) after-name)
              fctx (if fields
                     (reduce (fn [c f] (bind-pattern f c st :field (tree/node-range node)))
                             ctx'
                             (tree/children fields))
                     ctx')]
          (doseq [n after-name]
            (cond
              (= :list (tree/tag n))
              (walk-fn-tail (rest (tree/children n)) fctx st :method-param)
              ;; protocol and interface names
              (tree/symbol-node? n) (add-occ! ctx' st n false))))

        "declare"
        (doseq [n (rest cs)]
          (when (tree/symbol-node? n)
            (let [nm2 (sym-text n)]
              (add-var! ctx st
                     (var-entry ctx nm2 :declare nil nil [] "declare" false
                                (tree/node-range n) (tree/node-range n))))))

        ;; def, defonce, defmulti: walk the value/dispatch form
        (doseq [n (if (attr-map h after-doc) (rest after-doc) after-doc)]
          (walk-node n ctx' st))))))

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
                      (some-> (get (:aliases ctx) (first (string/split (str mm-text) #"/")))
                              (= "integrant.core")))]
          (add-keyword! ctx st dispatch (boolean ig?)))
        (walk-node dispatch ctx st)))
    ;; (defmethod m :k fn-name [x] ...) names the fn
    (let [after (if (and (tree/symbol-node? (first after))
                         (second after))
                  (rest after)
                  after)]
      (walk-fn-tail after ctx st))))

(defn- handle-let [node ctx st h]
  (let [cs (tree/children node)
        v (second cs)
        body (drop 2 cs)
        scope (tree/node-range node)]
    (if-not (and v (= :vector (tree/tag v)))
      (walk-body (rest cs) ctx st)
      (cond
        (#{"binding" "with-local-vars"} h)
        (do
          (doseq [[sym init] (partition-all 2 (tree/children v))]
            ;; binding names dynamic vars; with-local-vars binds locals
            (if (= h "binding")
              (walk-node sym ctx st)
              nil)
            (when init (walk-node init ctx st)))
          (walk-body body
                     (if (= h "with-local-vars")
                       (reduce (fn [c [sym _]] (bind-pattern sym c st :let scope))
                               ctx (partition-all 2 (tree/children v)))
                       ctx)
                     st))

        (contains? #{"for" "doseq"} h)
        (walk-body body (bind-for-vector v ctx st (keyword h) scope) st)

        :else
        (walk-body body (bind-let-vector v ctx st (keyword h) scope) st)))))

(defn- handle-fn [node ctx st]
  (let [cs (tree/children node)
        after (rest cs)
        maybe-name (first after)
        named? (tree/symbol-node? maybe-name)
        nm (when named? (sym-text maybe-name))
        ctx' (if nm
               (bind-local ctx nm (add-local! st maybe-name :fn (tree/node-range node) false false))
               ctx)]
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
                           (bind-local c nm (add-local! st n :letfn (tree/node-range node) false false))
                           c)))
                     ctx
                     specs)]
    (doseq [spec specs]
      (walk-fn-tail (rest (tree/children spec)) ctx' st))
    (walk-body body ctx' st)))

(defn- handle-methods [ctx st methods]
  (doseq [m methods]
    (cond
      (= :list (tree/tag m))
      (walk-fn-tail (rest (tree/children m)) ctx st :method-param)
      ;; protocol, interface and type names between the methods
      (tree/symbol-node? m) (add-occ! ctx st m false))))

(defn- handle-case [node ctx st]
  (let [[_ expr & clauses] (tree/children node)]
    (when expr (walk-node expr ctx st))
    ;; test constants are literals, never evaluated
    (doseq [[test-or-default result] (partition-all 2 clauses)]
      (if result
        (walk-node result ctx st)
        (walk-node test-or-default ctx st)))))

(defn- handle-catch [node ctx st]
  (let [[_ klass binding & body] (tree/children node)]
    (when klass (add-occ! ctx st klass false))
    (walk-body body (if binding
                      (bind-pattern binding ctx st :catch (tree/node-range node))
                      ctx)
               st)))

(defn- handle-dot
  "(. target member args*), (. target (member args*)) and (.. target member*):
  member names are not vars, their arguments are walked."
  [node ctx st h]
  (let [[_ target & members] (tree/children node)
        member! (fn [m]
                  (when (= :list (tree/tag m))
                    (walk-body (rest (tree/children m)) ctx st)))]
    (when target (walk-node target ctx st))
    (if (= h ".")
      (let [[m & args] members]
        (member! m)
        (walk-body args ctx st))
      (doseq [m members] (member! m)))))

(defn- handle-as-> [node ctx st]
  (let [cs (tree/children node)
        expr (second cs)
        nm-node (nth cs 2 nil)
        nm (sym-text nm-node)
        _ (when expr (walk-node expr ctx st))
        ctx' (if nm
               (bind-local ctx nm (add-local! st nm-node :as-> (tree/node-range node) false false))
               ctx)]
    (walk-body (drop 3 cs) ctx' st)))

(def ^:private empty-ns-meta
  {:aliases {} :refers {} :requires [] :require-specs []
   :refer-sites [] :imports {} :refer-all [] :as-aliases []
   :core-excludes []})

(defn- import-entries
  "[[simple fq] ...] for one :import item: a symbol or a (pkg Class ...) list."
  [lib]
  (let [n (tree/meta-form lib)]
    (cond
      (tree/symbol-node? n)
      (let [fq (sym-text n)]
        [[(last (string/split fq #"\.")) fq]])

      (#{:list :vector} (tree/tag n))
      (let [[pkg & classes] (keep sym-text (tree/children n))]
        (for [c classes] [c (str pkg "." c)]))

      :else [])))

(defn- active-branch
  "The value of the reader-conditional branch Jolt reads, or nil."
  [rc-node]
  (let [body (last (tree/children rc-node))
        pairs (partition-all 2 (tree/children body))
        by-key (into {} (map (fn [[k v]] [(tree/text k) v])) pairs)]
    (some by-key active-features)))

(defn- rc-node? [node]
  (and (= :reader-macro (tree/tag node))
       (string/starts-with? (tree/text node) "#?")))

(defn- splicing? [node]
  (string/starts-with? (tree/text node) "#?@"))

(defn- clause-items
  "A clause's items with reader conditionals resolved to the active branch."
  [items]
  (mapcat (fn [n]
            (if (rc-node? n)
              (when-let [v (active-branch n)]
                (if (splicing? n) (tree/children v) [v]))
              [n]))
          items))

(defn- handle-ns [node ctx _st]
  (let [cs (tree/children node)
        nm-node (second cs)
        nm (some-> nm-node tree/meta-form sym-text)
        clauses (clause-items (drop 2 cs))]
    (if nm
      (let [st-meta (atom empty-ns-meta)]
        (doseq [clause clauses]
          (when (= :list (tree/tag clause))
            (let [ccs (tree/children clause)
                  kw (tree/text (first ccs))
                  items (clause-items (rest ccs))]
              (case kw
                (":require" ":use")
                (doseq [lib items
                        spec (expand-libspec lib nil)]
                  (let [spec (cond-> spec (= kw ":use") (assoc :refer-all true :bare false))]
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
                    (swap! st-meta update :refer-sites into (:refer-sites spec))))

                ":import"
                (doseq [lib items
                        [simple fq] (import-entries lib)]
                  (swap! st-meta assoc-in [:imports simple] fq))

                ":refer-clojure"
                (doseq [[k v] (partition-all 2 items)]
                  (when (and v (= ":exclude" (tree/text k)))
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

(defn- handle-in-ns [node ctx _st]
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
        ;; the heads of the calls around an occurrence: if one is a macro,
        ;; its arguments need not be code
        (let [ctx (if-let [h (sym-text head)]
                    (update ctx :heads (fnil conj []) h)
                    ctx)]
          (doseq [n (rest cs)] (walk-node n ctx st)))))))

(defn- head-fqn
  "The fqn a call head names in ctx, as far as the ns form tells."
  [ctx h]
  (let [slash (string/index-of h "/")]
    (cond
      (and slash (pos? slash))
      (let [p (subs h 0 slash)]
        (str (get (:aliases ctx) p p) "/" (subs h (inc slash))))
      (get (:refers ctx) h) (get (:refers ctx) h)
      :else (str (:ns ctx) "/" h))))

(defn- walk-list*
  [node ctx st]
  (let [raw (some-> node (tree/children) first sym-text)
        h (or (when (and raw (seq *lint-as*) (not (get (:locals ctx) raw)))
                (get *lint-as* (head-fqn ctx raw)))
              raw)
        ;; record the head as an occurrence for special handlers too, so
        ;; core macros count as used and navigate
        head! #(add-occ! ctx st (first (tree/children node)) true)]
    (cond
      ;; rich comments: references count, definitions do not
      (= h "comment") (do (head!)
                          (walk-body (rest (tree/children node)) (assoc ctx :in-comment true) st))
      (= h "quote") (when-let [n (second (tree/children node))]
                      (walk-quoted n ctx st))
      (= h "defmethod") (do (head!) (handle-defmethod node ctx st))
      (contains? #{"fn" "fn*"} h) (do (head!) (handle-fn node ctx st))
      (= h "letfn") (do (head!) (handle-letfn node ctx st))
      (= h "reify") (do (head!) (handle-methods ctx st (rest (tree/children node))))
      (contains? #{"extend-type" "extend-protocol"} h)
      (let [cs (tree/children node)]
        (head!)
        (when (second cs) (walk-node (second cs) ctx st))
        (handle-methods ctx st (drop 2 cs)))
      (= h "as->") (do (head!) (handle-as-> node ctx st))
      (= h "case") (do (head!) (handle-case node ctx st))
      (= h "catch") (handle-catch node ctx st)
      (contains? #{"." ".."} h) (handle-dot node ctx st h)
      (and h (contains? binding-heads h)) (do (head!) (handle-let node ctx st h))
      (and h (contains? def-kinds h)) (do (head!) (handle-def node ctx st h))
      :else (walk-call node ctx st))))

(defn- walk-list
  "Walk a list form. Returns the new ctx for forms that change it (ns,
  in-ns), else nil."
  [node ctx st]
  (let [h (some-> node (tree/children) first sym-text)]
    (case h
      "ns" (handle-ns node ctx st)
      "in-ns" (handle-in-ns node ctx st)
      (do (walk-list* node ctx st) nil))))

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

(defn- walk-reader-conditional [node ctx st]
  (let [body (last (tree/children node))
        active (active-branch node)]
    (doseq [[_ v] (partition-all 2 (tree/children body))
            :when v]
      (walk-node v (if (identical? v active) ctx (assoc ctx :inactive true)) st))))

(defn- walk-node [node ctx st]
  (when node
    (let [tag (tree/tag node)]
      (cond
        (tree/uneval? node) nil
        (tree/keyword-node? node) (add-keyword! ctx st node false)
        (= :token tag) (add-occ! ctx st node false)
        (= :quote tag) (doseq [c (tree/children node)] (walk-quoted c ctx st))
        (= :syntax-quote tag)
        (doseq [c (tree/children node)] (walk-node c (assoc ctx :syntax-quote true) st))
        (contains? #{:unquote :unquote-splicing} tag)
        (doseq [c (tree/children node)] (walk-node c (dissoc ctx :syntax-quote) st))
        (= :meta tag) (walk-node (tree/meta-form node) ctx st)
        (#{:list :fn} tag) (walk-list node ctx st)
        (= :reader-macro tag)
        (if (rc-node? node)
          (walk-reader-conditional node ctx st)
          ;; a tagged literal: #inst "...", #my/tag {...}
          (doseq [c (rest (tree/children node))] (walk-node c ctx st)))
        (contains? collection-tags tag)
        (doseq [c (tree/children node)] (walk-node c ctx st))
        :else nil))))

;; ---------------------------------------------------------------------------
;; libspecs

(defn- libspec-target [node]
  (let [n (tree/meta-form node)]
    (when (tree/symbol-node? n) (sym-text n))))

(defn- expand-libspec
  "Require specs for one libspec node. `prefix` is the enclosing prefix-list
  namespace, if any."
  [node prefix]
  (let [n (tree/meta-form node)
        full (fn [t] (if prefix (str prefix "." t) t))]
    (cond
      (tree/symbol-node? n)
      [{:target (full (sym-text n)) :range (tree/node-range n) :bare true}]

      (and (= :vector (tree/tag n))
           ;; [lib :opt v ...]; [prefix sub ...] is a prefix vector instead
           (let [opt (second (tree/children n))]
             (or (nil? opt) (tree/keyword-node? opt))))
      (let [cs (tree/children n)
            target (some-> (libspec-target (first cs)) full)
            opts (partition-all 2 (rest cs))]
        (when target
          [(reduce
            (fn [spec [k v]]
              (let [kt (tree/text k)]
                (case kt
                  ":as" (assoc spec :alias (sym-text (tree/meta-form v)))
                  ":as-alias" (assoc spec :alias (sym-text (tree/meta-form v)) :as-alias true)
                  ":refer" (if (= ":all" (tree/text v))
                             (assoc spec :refer-all true)
                             (let [entries (vec (keep (fn [x]
                                                        (let [xm (tree/meta-form x)
                                                              nm (sym-text xm)]
                                                          (when nm
                                                            {:name nm
                                                             :target (str target "/" nm)
                                                             :range (tree/node-range xm)})))
                                                      (tree/children v)))]
                               (assoc spec
                                      :refers (into (or (:refers spec) {})
                                                    (map (juxt :name :target) entries))
                                      :refer-sites entries)))
                  ":rename" (let [renames (keep (fn [[a b]]
                                                  (let [an (sym-text (tree/meta-form a))
                                                        bn (sym-text (tree/meta-form b))]
                                                    (when (and an bn) [bn (str target "/" an)])))
                                                (partition-all 2 (tree/children v)))]
                              (update spec :refers #(into (or % {}) renames)))
                  spec)))
            {:target target :range (tree/node-range n) :bare (empty? opts)}
            opts)]))

      (and (contains? #{:list :vector} (tree/tag n))
           (seq (tree/children n)))
      (let [cs (tree/children n)
            p (some-> (libspec-target (first cs)) full)]
        (mapcat #(expand-libspec % p) (clause-items (rest cs))))

      :else [])))

;; ---------------------------------------------------------------------------
;; entry

(defn- walk-top [node ctx st]
  (let [n (tree/meta-form node)]
    (cond
      (tree/uneval? n) ctx
      ;; #?(:clj (ns ...)) and friends: the active branch can change the ctx
      (rc-node? n)
      (let [active (active-branch n)]
        (doseq [[_ v] (partition-all 2 (tree/children (last (tree/children n))))
                :when (and v (not (identical? v active)))]
          (walk-node v (assoc ctx :inactive true) st))
        (cond
          (nil? active) ctx
          (splicing? n) (reduce #(walk-top %2 %1 st) ctx (tree/children active))
          :else (walk-top active ctx st)))
      (= :list (tree/tag n)) (or (walk-list n ctx st) ctx)
      :else (do (walk-node n ctx st) ctx))))

(def ^:private shallow-depth
  "Deep enough for a definition's name, docstring, params (also per arity
  and per protocol method) and a ns form's libspecs."
  4)

(defn analyze-shallow
  "Definitions and the ns form only: for dependency files, where nothing
  else is indexed. Bodies are skimmed, not parsed."
  [source path]
  (binding [read/*skim-depth* shallow-depth]
    (assoc (analyze* source path)
           :occurrences [] :keywords [] :locals [] :local-refs [])))

(defn analyze
  "Parse source and return the analysis map."
  [source path]
  (analyze* source path))

(defn- analyze*
  [source path]
  (let [parsed (read/parse-text source)
        st (new-state)
        ctx {:ns nil
             :ns-meta empty-ns-meta
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
     :ns-meta empty-ns-meta
     :vars []
     :occurrences []
     :keywords @(:keywords st)
     :locals []
     :local-refs []
     :parse-ok (:ok parsed)
     :parse-error (:error parsed)}))
