(ns jolt-lsp.spec.text-spec
  "Text geometry: offsets, lines, and the two column systems.

  Internal rows and columns are 1-based and count codepoints. LSP columns
  are 0-based and count UTF-16 code units. Byte offsets are UTF-8."
  (:require [writ.spec :refer [spec ann graph law]]))

(spec jolt-lsp.text {:require :tested})

(ann line-starts [String -> (List Nat)])
(ann offset->position [String Nat -> (Tuple Nat Nat)])
(ann position->offset [String (Tuple Nat Nat) -> Nat])
(ann line-utf16-length [String Nat -> Nat])
(ann utf16->codepoint-col [String Nat Nat -> Nat])
(ann codepoint->utf16-col [String Nat Nat -> Nat])
(ann utf8-offset [String Nat -> Nat])

(graph text-geometry
  {:start :text
   :states {:text String
            :offsets (List Nat)
            :position (Tuple Nat Nat)
            :count Nat}
   :edges {:text {[line-starts] #{:offsets}
                  [offset->position Nat] #{:position}
                  [position->offset (Tuple Nat Nat)] #{:count}
                  [line-utf16-length Nat] #{:count}
                  [utf16->codepoint-col Nat Nat] #{:count}
                  [codepoint->utf16-col Nat Nat] #{:count}
                  [utf8-offset Nat] #{:count}}}
   :final [:offsets :position :count]})

(law line-starts-start-at-zero
  (forall [s String] (= 0 (first (line-starts s)))))

(law line-starts-in-bounds
  (forall [s String]
    (every? (fn [n] (<= n (count s))) (line-starts s))))

(law offset-position-roundtrip
  (forall [s String, n Nat]
    (=> (<= n (count s))
        (= n (position->offset s (offset->position s n))))))

(law offset-position-at-start
  (forall [s String] (= [1 1] (offset->position s 0))))

(law utf16-codepoint-roundtrip
  (forall [s String, row Nat, col Nat]
    (=> (and (>= row 1)
             (<= row (count (line-starts s)))
             (<= col (line-utf16-length s row)))
        (= col (codepoint->utf16-col s row (utf16->codepoint-col s row col))))))

(law utf8-offset-monotone
  (forall [s String, n Nat, m Nat]
    (=> (and (<= n m) (<= m (count s)))
        (<= (utf8-offset s n) (utf8-offset s m)))))

(law empty-line-starts (= (line-starts "") [0]))
(law empty-offset (= (offset->position "" 0) [1 1]))
(law empty-utf8 (= (utf8-offset "" 0) 0))
(law offset-past-end (= (offset->position "abc" 99) [1 4]))
(law row-below-one-clamps (= (position->offset "abc" [0 1]) 0))
(law line-starts-simple (= (line-starts "a\nb") [0 2]))
(law line-starts-crlf (= (line-starts "a\r\nb") [0 3]))
(law offset-start (= (offset->position "abc" 0) [1 1]))
(law offset-newline (= (offset->position "a\nb" 1) [1 2]))
(law offset-second-line (= (offset->position "a\nb" 2) [2 1]))
(law astral-codepoint-column (= (offset->position "😀x" 2) [1 3]))
(law utf16-after-astral (= (utf16->codepoint-col "😀x" 1 2) 1))
(law utf16-after-x (= (utf16->codepoint-col "😀x" 1 3) 2))
(law line-utf16-astral (= (line-utf16-length "😀x" 1) 3))
(law utf8-astral (= (utf8-offset "😀" 1) 4))
