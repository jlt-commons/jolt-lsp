(ns jolt-lsp.spec.resolve-spec
  "Symbol resolution over a file's analysis: local-free name lookup.

    alias/name  -> the target namespace's var
    referred    -> the refer target
    bare        -> current namespace, then core
    otherwise   -> unresolved

  Locals are resolved structurally in the analysis walker; this spec covers
  the namespace-level lookup."
  (:require [writ.spec :refer [spec ann graph law refine]]))

(spec jolt-lsp.resolve {:require :tested})

(refine Analysis [a {:ns-name (Opt String)
                     :var-names (Set String)
                     :aliases (Map String String)
                     :refers (Map String String)
                     :refer-all (Map String String)
                     :imports (Map String String)
                     :requires (Set String)
                     :core (Set String)}] true)

(refine Resolution [r {:kind Keyword
                       :fqn (Opt String)}] true)

(ann resolve-name [Analysis String String -> Resolution])

(graph resolving
  {:start :analysis
   :states {:analysis Analysis
            :resolution Resolution}
   :edges {:analysis {[resolve-name String String] #{:resolution}}}
   :final [:resolution]})

(defn sample-analysis []
  {:ns-name "app.core"
   :var-names #{"f"}
   :aliases {"u" "app.util"}
   :refers {"r" "app.util/g"}
   :refer-all {"h" "app.lib/h"}
   :imports {"Date" "java.util.Date"}
   :requires #{"app.util"}
   :core #{"map"}})

(law current-ns-var
  (= "app.core/f" (:fqn (resolve-name (sample-analysis) "app.core" "f"))))

(law alias-qualified
  (= "app.util/g" (:fqn (resolve-name (sample-analysis) "app.core" "u/g"))))

(law referred-name
  (= "app.util/g" (:fqn (resolve-name (sample-analysis) "app.core" "r"))))

(law core-name
  (= :core (:kind (resolve-name (sample-analysis) "app.core" "map"))))

(law refer-all-name
  (= "app.lib/h" (:fqn (resolve-name (sample-analysis) "app.core" "h"))))

(law imported-class
  (= :class (:kind (resolve-name (sample-analysis) "app.core" "Date"))))

(law unresolved-name
  (= :unresolved (:kind (resolve-name (sample-analysis) "app.core" "nope"))))

(law no-current-ns-unresolved
  (= :unresolved
     (:kind (resolve-name (assoc (sample-analysis) :ns-name nil) "app.core" "f"))))

(law empty-name-unresolved
  (= :unresolved (:kind (resolve-name (sample-analysis) "app.core" ""))))

(law resolves-to-a-fqn-or-is-unresolved
  (forall [ns String, name String]
    (let [r (resolve-name (sample-analysis) ns name)]
      (or (= :unresolved (:kind r))
          (string? (:fqn r))))))
