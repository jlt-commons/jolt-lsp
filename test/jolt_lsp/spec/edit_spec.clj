(ns jolt-lsp.spec.edit-spec
  "Pure range edits: offsets, old text, and application."
  (:require [writ.spec :refer [spec ann graph law refine]]))

(spec jolt-lsp.edit.vedit {:require :tested})

(refine Edit [e {:start-offset Nat
                 :end-offset Nat
                 :new-text String}] true)

(ann range->edit [String (Tuple Nat Nat Nat Nat) String -> Edit])
(ann apply-edits [String (List Edit) -> String])

(graph editing
  {:start :text
   :states {:text String
            :edit Edit}
   :edges {:text {[range->edit (Tuple Nat Nat Nat Nat) String] #{:edit}
                  [apply-edits (List Edit)] #{:text}}}
   :final [:edit]})

(law no-edits-identity
  (forall [s String] (= s (apply-edits s []))))

(law empty-insert-identity
  (forall [s String]
    (= s (apply-edits s [(range->edit s [1 1 1 1] "")]))))

(law replace-concrete
  (= "Xbc" (apply-edits "abc" [{:start-offset 0 :end-offset 1 :new-text "X"}])))

(law delete-concrete
  (= "bc" (apply-edits "abc" [{:start-offset 0 :end-offset 1 :new-text ""}])))

(law two-edits
  (= "aXY" (apply-edits "abc" [{:start-offset 1 :end-offset 2 :new-text "X"}
                               {:start-offset 2 :end-offset 3 :new-text "Y"}])))

(law range-edit-mid
  (= "aXc" (apply-edits "abc" [(range->edit "abc" [1 2 1 3] "X")])))

(law range-edit-astral
  (let [e (range->edit "😀x" [1 1 1 2] "Z")]
    (and (= 0 (:start-byte e))
         (= 4 (:end-byte e))
         (= "😀" (:old-text e)))))

(law range-edit-pins-new-text
  (forall [s String, t String]
    (let [e (range->edit s [1 1 1 1] t)]
      (and (= t (:new-text e))
           (= 0 (:start-offset e))
           (= 0 (:end-offset e))))))

(law zero-range-inserts
  (forall [s String, t String]
    (= (str t s) (apply-edits s [(range->edit s [1 1 1 1] t)]))))
