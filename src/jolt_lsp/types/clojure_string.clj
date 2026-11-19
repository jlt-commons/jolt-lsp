;; Copied from clj-kondo (https://github.com/clj-kondo/clj-kondo),
;; src/clj_kondo/impl/types/clojure/string.clj, Copyright (c) Michiel Borkent,
;; distributed under the Eclipse Public License 1.0. Only the namespace
;; and its requires are changed. See NOTICE.
(ns jolt-lsp.types.clojure-string
  "clj-kondo's type specs for clojure.string."
  {:no-doc true})

(def clojure-string
  {;; 75
   'replace
   {:arities {3 {:args [:char-sequence #{:string :char :regex} #{:string :char :ifn}]
                 :ret :string}}}
   ;; 180
   'join
   {:arities {1 {:args [:seqable]
                 :ret :string}
              2 {:args [:any :seqable]
                 :ret :string}}}
   ;; 360
   'starts-with?
   {:arities {2 {:args [:char-sequence :string]
                 :ret :boolean}}}
   ;; 366
   'ends-with?
   {:arities {2 {:args [:char-sequence :string]
                 :ret :boolean}}}
   ;; 372
   'includes?
   {:arities {2 {:args [:char-sequence :char-sequence]
                 :ret :boolean}}}})
