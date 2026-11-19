;; Adapted from clj-kondo's clj-kondo.impl.types
;; (https://github.com/clj-kondo/clj-kondo, src/clj_kondo/impl/types.clj),
;; Copyright (c) Michiel Borkent, distributed under the Eclipse Public
;; License 1.0: known-types, is-a-relations, could-be-relations, labels,
;; nilable?, unnil, label, match?, tag->label, tag-from-meta, called-arity
;; and the argument walk of lint-arg-types. See NOTICE.
(ns jolt-lsp.types
  "Argument types against clj-kondo's specs for clojure.core, clojure.string
  and clojure.set. Pure.

  clj-kondo infers types through locals, return types and its cache; this
  takes the types the walk can see at a call site, literals and the return
  types of nested core calls, and anything else is :any, which matches
  every spec. So a finding means the argument as written cannot be what
  the fn takes."
  (:require [clojure.string :as string]
            [jolt-lsp.types.clojure-core :as core]
            [jolt-lsp.types.clojure-set :as cset]
            [jolt-lsp.types.clojure-string :as cstring]))

(def ^:private specs
  {"clojure.core" core/clojure-core
   "clojure.string" cstring/clojure-string
   "clojure.set" cset/clojure-set})

(def ^:private known-types
  #{:string :char-sequence :seqable :int :long :short :number :pos-int
    :nat-int :neg-int :double :byte :ratio :vector :sequential :associative
    :coll :ideref :ifn :stack :map :nil :set :sorted-set :fn :keyword :symbol
    :transducer :list :seq :sorted-map :boolean :true :false :truthy :atom
    :future :regex :char :seqable-or-transducer :throwable :any :float :var
    :ilookup :array :inst :class})

(def ^:private is-a-relations
  {:string #{:char-sequence :seqable}
   :char-sequence #{:seqable}
   :int #{:number}
   :long #{:number}
   :short #{:number}
   :pos-int #{:int :nat-int :number}
   :nat-int #{:int :number}
   :neg-int #{:int :number}
   :double #{:number}
   :float #{:number}
   :byte #{:number}
   :ratio #{:number}
   :vector #{:seqable :sequential :associative :coll :ifn :stack :ilookup}
   :map #{:seqable :associative :coll :ifn :ilookup}
   :nil #{:seqable}
   :coll #{:seqable}
   :set #{:seqable :coll :ifn :ilookup}
   :sorted-set #{:set :seqable :coll :ifn :ilookup}
   :fn #{:ifn}
   :keyword #{:ifn}
   :symbol #{:ifn}
   :associative #{:seqable :coll :ifn :ilookup}
   :transducer #{:ifn :fn}
   :list #{:seq :sequential :seqable :coll :stack}
   :seq #{:seqable :sequential :coll}
   :sequential #{:coll :seqable}
   :sorted-map #{:map :seqable :associative :coll :ifn :ilookup}
   :true #{:boolean}
   :false #{:boolean}
   :atom #{:ideref}
   :future #{:ideref}
   :var #{:ideref :ifn}
   :array #{:seqable :ilookup}})

(def ^:private could-be-relations
  {:boolean #{:true :false}
   :char-sequence #{:string}
   :int #{:neg-int :nat-int :pos-int :long :short :float :double :number}
   :long #{:int :neg-int :nat-int :pos-int :short :float :double :number}
   :short #{:int :long :neg-int :nat-int :pos-int :float :double :number}
   :pos-int #{:long :short :float :double :number}
   :nat-int #{:pos-int :long :short :float :double :number}
   :neg-int #{:long :short :float :double :number}
   :byte #{:int :long :short :float :double :number}
   :float #{:double :number}
   :double #{:float :number}
   :number #{:neg-int :pos-int :nat-int :int :long :short :double :byte :ratio :float}
   :coll #{:map :sorted-map :vector :set :sorted-set :list :associative :seq
           :sequential :ifn :stack :ilookup}
   :seqable #{:coll :vector :set :sorted-set :map :associative
              :char-sequence :string :nil
              :list :seq :sequential :ifn :stack :sorted-map :ilookup :array}
   :associative #{:map :vector :sequential :stack :sorted-map}
   :ifn #{:fn :transducer :symbol :keyword :map :set :sorted-set :vector
          :associative :seqable :coll :sequential :stack :sorted-map :var
          :ideref :ilookup}
   :fn #{:transducer}
   :seq #{:list :stack}
   :stack #{:list :vector :seq :sequential :seqable :coll :ifn :associative :ilookup}
   :sequential #{:seq :list :vector :ifn :associative :stack :ilookup}
   :map #{:sorted-map}
   :set #{:sorted-set}
   :ideref #{:atom :future :var :ifn}
   :ilookup #{:map :set :sorted-set :sorted-map :coll :seqable :ifn :associative
              :vector :sequential :stack :array}})

(def ^:private labels
  {:nil "nil" :string "string" :number "number" :int "integer" :long "long"
   :short "short" :double "double" :float "float" :pos-int "positive integer"
   :nat-int "natural integer" :neg-int "negative integer" :byte "byte"
   :ratio "ratio" :seqable "seqable collection" :seq "seq" :vector "vector"
   :stack "stack (list, vector, etc.)" :associative "associative collection"
   :map "map" :coll "collection" :list "list" :regex "regular expression"
   :char "character" :boolean "boolean" :true "boolean" :false "boolean"
   :truthy "truthy value" :atom "atom" :future "future" :ideref "deref"
   :fn "function" :ifn "function" :keyword "keyword" :symbol "symbol"
   :transducer "transducer" :seqable-or-transducer "seqable or transducer"
   :set "set" :sorted-set "sorted set" :char-sequence "char sequence"
   :sequential "sequential collection" :throwable "throwable"
   :sorted-map "sorted map" :var "var" :ilookup "ILookup" :array "array"
   :class "class" :inst "instant"})

(defn- nilable? [k]
  (and (keyword? k) (= "nilable" (namespace k))))

(defn- unnil [k]
  (if (nilable? k) (keyword (name k)) k))

(defn- always-falsy? [t]
  (contains? #{:nil :false} t))

(defn- keyword-match? [actual expected]
  (or (identical? actual expected)
      (identical? actual :any)
      (and (identical? actual :truthy) (not (always-falsy? expected)))
      (identical? expected :any)
      (contains? (get is-a-relations actual) expected)
      (contains? (get could-be-relations actual) expected)
      (let [k (unnil actual)
            target (unnil expected)]
        (or (identical? k target)
            (identical? :any target)
            (contains? (get is-a-relations k) target)
            (contains? (get could-be-relations k) target)
            (not (contains? known-types k))))
      (and (identical? actual :nil) (or (nilable? expected)
                                        (identical? :seqable expected)))))

(defn match?
  "Whether a value of type `actual` could be one of type `expected`. A set
  is a union: it matches when a member does."
  [actual expected]
  (let [one (fn [t] (if (keyword? t) (keyword-match? t expected) true))]
    (boolean
     (cond
       (keyword? actual) (keyword-match? actual expected)
       (map? actual) (one (:type actual))
       (set? actual) (some one actual)
       :else true))))

(defn- label [k]
  (let [k (if (map? k) (:type k) k)]
    (if (nilable? k)
      (str (get labels (unnil k)) " or nil")
      (get labels k))))

(defn tag->label [x]
  (let [label-fn #(or (label %) (name %))]
    (cond (keyword? x) (label-fn x)
          (set? x) (string/join " or " (map label-fn (sort-by str x)))
          (map? x) "map"
          :else (str x))))

(defn literal-tag
  "The type of a literal node ({:tag :kind :text}); :any for anything else."
  [{:keys [tag kind text]}]
  (case tag
    :string :string
    :regex :regex
    :keyword :keyword
    :vector :vector
    :map :map
    :set :set
    :fn :fn
    :token (case kind
             :number (let [v (try (read-string text) (catch Throwable _ nil))]
                       (cond (not (number? v)) :number
                             (ratio? v) :ratio
                             (integer? v) (cond (pos? v) :pos-int (zero? v) :nat-int :else :neg-int)
                             :else :double))
             :char :char
             :literal (case text
                        "nil" :nil
                        ("true" "false") :boolean
                        :any)
             :any)
    :any))

(defn tag-from-meta
  "The type a type hint names (^String, ^long), or nil. A class hint is
  nilable: Clojure cannot say a String is never nil."
  [hint]
  (case (str hint)
    "void" :nil
    "boolean" :boolean
    ("Boolean" "java.lang.Boolean") :nilable/boolean
    "byte" :byte
    ("Byte" "java.lang.Byte") :nilable/byte
    ("Number" "java.lang.Number") :nilable/number
    "int" :int
    ("Integer" "java.lang.Integer") :nilable/int
    "long" :long
    ("Long" "java.lang.Long") :nilable/long
    "short" :short
    ("Short" "java.lang.Short") :nilable/short
    "double" :double
    "float" :float
    ("Double" "java.lang.Double") :nilable/double
    ("Float" "java.lang.Float") :nilable/float
    "ratio?" :ratio
    ("CharSequence" "java.lang.CharSequence") :nilable/char-sequence
    ("String" "java.lang.String") :nilable/string
    "char" :char
    ("Character" "java.lang.Character") :nilable/char
    ("Seqable" "clojure.lang.Seqable") :seqable
    "java.util.List" :nilable/list
    "class" :class
    ("Class" "java.lang.Class") :nilable/class
    ("Date" "java.util.Date") :nilable/inst
    ("Future" "java.util.concurrent.Future") :nilable/future
    "future" :future
    nil))

(defn- spec-of
  "The spec for fqn: from `extra` ({fqn spec}: the user's :type-mismatch
  config, then types the project's own fns declare), else the built-ins."
  [extra fqn]
  (or (get extra fqn)
      (when-let [i (some-> fqn (string/index-of "/"))]
        (get-in specs [(subs fqn 0 i) (symbol (subs fqn (inc i)))]))))

(defn- called-arity [arities arity]
  (or (get arities arity)
      (when-let [v (:varargs arities)]
        (if-let [ma (:min-arity v)]
          (when (>= arity ma) v)
          v))))

(defn ret-tag
  "The type a call to fqn with n args returns, when the spec says; :any
  otherwise."
  [extra fqn n]
  (let [r (:ret (called-arity (:arities (spec-of extra fqn)) n))]
    (if (or (keyword? r) (set? r)) r :any)))

(defn- non-match [i s t]
  {:index i :message (str "Expected: " (tag->label s) ", received: " (tag->label t) ".")})

(defn- expected-types
  "The spec type for each of n arguments, from an :args vector: its
  positional types, then a {:op :rest :spec s :last l} for the rest (l for
  the last one). nil is any type."
  [args-spec n]
  (let [flat (flatten args-spec)
        positional (vec (take-while #(not (:op %)) flat))
        rest-op (some #(when (= :rest (:op %)) %) flat)
        extra (- n (count positional))]
    (if (and rest-op (pos? extra))
      (vec (concat positional
                   (repeat (dec extra) (:spec rest-op))
                   [(or (:last rest-op) (:spec rest-op))]))
      (vec (take n positional)))))

(defn mismatches
  "For a call to fqn with arguments of these types, the ones no type of the
  spec admits: [{:index n :message ...}]."
  [extra fqn tags]
  (let [tags (vec tags)
        args-spec (:args (called-arity (:arities (spec-of extra fqn)) (count tags)))]
    (if-not (sequential? args-spec)
      []
      (vec (for [[i s] (map-indexed vector (expected-types args-spec (count tags)))
                 :let [t (get tags i)]
                 :when (cond (set? s) (not (some #(match? t %) s))
                             (keyword? s) (not (match? t s))
                             :else false)]
             (non-match i s t))))))
