(ns jolt-lsp.spec.read-spec
  "Safe reading: total, and it classifies ok from error without throwing."
  (:require [writ.spec :refer [spec ann graph law]]))

(spec jolt-lsp.read {:require :tested})

(ann parse [String -> Any])
(ann valid? [String -> Bool])
(ann error-message [String -> (Opt String)])
(ann top-level-count [String -> Nat])

(graph reading
  {:start :source
   :states {:source String
            :result Any
            :ok Bool
            :error (Opt String)
            :count Nat}
   :edges {:source {[parse] #{:result}
                    [valid?] #{:ok}
                    [error-message] #{:error}
                    [top-level-count] #{:count}}}
   :final [:result :ok :error :count]})

(law parse-always-returns-a-value
  (forall [s String] (some? (parse s))))

(law empty-is-valid
  (valid? ""))

(law ns-is-valid
  (valid? "(ns app.core)"))

(law unbalanced-is-invalid
  (not (valid? "(defn f [x")))

(law valid-has-no-error
  (nil? (error-message "(ns app.core)")))

(law invalid-has-an-error
  (some? (error-message "(defn f [x")))

(law broken-input-has-no-forms
  (= 0 (top-level-count "(")))

(law two-top-level-forms
  (= 2 (top-level-count "(ns a)\n(defn f [] 1)")))

(law empty-top-level-count (= (top-level-count "") 0))
(law empty-message (nil? (error-message "")))

(law parse-shape
  (forall [s String]
    (and (map? (parse s))
         (contains? (parse s) :ok)
         (contains? (parse s) :forms)
         (contains? (parse s) :count)
         (contains? (parse s) :error))))

(law valid-matches-parse
  (forall [s String] (= (valid? s) (:ok (parse s)))))

(law count-matches-parse
  (forall [s String] (= (top-level-count s) (:count (parse s)))))

(law error-matches-parse
  (forall [s String] (= (error-message s) (:error (parse s)))))
