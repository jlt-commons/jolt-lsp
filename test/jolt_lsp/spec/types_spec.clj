(ns jolt-lsp.spec.types-spec
  "Argument types against clj-kondo's core specs.

    a literal's tag      -> :string, :pos-int, :keyword, :vector, ...
    match? actual spec   -> could a value of type `actual` be a `spec`
    mismatches fqn tags  -> the arguments no spec type admits"
  (:require [writ.spec :refer [spec ann graph law refine]]))

(spec jolt-lsp.types {:require :tested})

(refine Tag [t Any] true)

(refine Literal [l {:tag Keyword :kind (Opt Keyword) :text String}] true)

(refine Mismatch [m {:index Nat :message String}] true)

(ann literal-tag [Literal -> Tag])
(ann match? [Tag Tag -> Bool])
(ann tag->label [Tag -> String])
(ann ret-tag [String Nat -> Tag])
(ann mismatches [String (List Tag) -> (List Mismatch)])

(graph typing
  {:start :literal
   :states {:literal Literal
            :tag Tag
            :verdict Bool
            :text String}
   :edges {:literal {[literal-tag] #{:tag}}
           :tag {[match? Tag] #{:verdict}
                 [tag->label] #{:text}}}
   :final [:verdict :text]})

(law literal-tags
  (and (= :string (literal-tag {:tag :string :kind :string :text "\"a\""}))
       (= :keyword (literal-tag {:tag :keyword :kind :keyword :text ":a"}))
       (= :pos-int (literal-tag {:tag :token :kind :number :text "3"}))
       (= :nat-int (literal-tag {:tag :token :kind :number :text "0"}))
       (= :neg-int (literal-tag {:tag :token :kind :number :text "-2"}))
       (= :double (literal-tag {:tag :token :kind :number :text "1.5"}))
       (= :nil (literal-tag {:tag :token :kind :literal :text "nil"}))
       (= :boolean (literal-tag {:tag :token :kind :literal :text "true"}))
       (= :char (literal-tag {:tag :token :kind :char :text "\\a"}))
       (= :vector (literal-tag {:tag :vector :kind nil :text "[]"}))
       (= :map (literal-tag {:tag :map :kind nil :text "{}"}))
       (= :set (literal-tag {:tag :set :kind nil :text "#{}"}))
       (= :regex (literal-tag {:tag :regex :kind :string :text "#\"a\""}))
       (= :fn (literal-tag {:tag :fn :kind nil :text "#(inc %)"}))))

(law symbols-are-unknown
  (= :any (literal-tag {:tag :token :kind :symbol :text "x"})))

(law any-matches-everything
  (forall [k Keyword]
    (and (match? :any k) (match? k :any))))

(law subtypes-match
  (and (match? :pos-int :number)
       (match? :vector :seqable)
       (match? :string :seqable)
       (match? :nil :seqable)
       (match? :nil :nilable/string)))

(law disjoint-types-do-not
  (and (not (match? :string :number))
       (not (match? :keyword :seqable))
       (not (match? :nil :number))))

(law union-matches-if-a-member-does
  (and (match? #{:string :nil} :string)
       (not (match? #{:string :keyword} :number))))

(law labels
  (and (= "number" (tag->label :number))
       (= "string or nil" (tag->label :nilable/string))
       (= "keyword or string" (tag->label #{:string :keyword}))))

(law inc-wants-a-number
  (= [{:index 0 :message "Expected: number, received: string."}]
     (mismatches "clojure.core/inc" [:string])))

(law rest-args-checked
  (= [1] (map :index (mismatches "clojure.core/+" [:pos-int :string :double]))))

(law matching-args-are-quiet
  (and (empty? (mismatches "clojure.core/inc" [:pos-int]))
       (empty? (mismatches "clojure.core/inc" [:any]))
       (empty? (mismatches "clojure.string/join" [:string :vector]))
       (empty? (mismatches "app.core/whatever" [:string]))))

(law join-wants-a-seqable-last
  (= [1] (map :index (mismatches "clojure.string/join" [:string :keyword]))))

(law return-tags
  (and (= :string (ret-tag "clojure.core/str" 2))
       (= :any (ret-tag "app.core/f" 1))))

(law unknown-types-never-mismatch
  (forall [fqn String, n Nat]
    (empty? (mismatches fqn (vec (repeat (min n 6) :any))))))

(law only-core-specs-apply
  (forall [nm String, n Nat]
    (and (empty? (mismatches (str "app.ns/" nm) (vec (repeat (min n 6) :string))))
         (= :any (ret-tag (str "app.ns/" nm) n)))))

(law mismatch-index-is-an-argument
  (forall [fqn String, tags (List Keyword)]
    (every? #(< (:index %) (count tags)) (mismatches fqn tags))))

(law ret-tag-from-spec
  (forall [n Nat]
    (and (= :string (ret-tag "clojure.core/str" n))
         (= :string (ret-tag "clojure.string/join" (inc (min n 1)))))))
