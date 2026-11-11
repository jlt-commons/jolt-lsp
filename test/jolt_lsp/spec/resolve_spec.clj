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

(refine VarDef [v {:name String
                   :ns String
                   :fqn String
                   :kind Keyword}] true)

(refine Analysis [a {:ns-name (Opt String)
                     :vars (List VarDef)
                     :aliases (Map String String)
                     :refers (Map String String)
                     :requires (List String)
                     :core (List String)}] true)

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
   :vars [{:name "f" :ns "app.core" :fqn "app.core/f" :kind :defn}
          {:name "g" :ns "app.util" :fqn "app.util/g" :kind :defn}]
   :aliases {"u" "app.util"}
   :refers {"r" "app.util/g"}
   :requires ["app.util"]
   :core ["map"]})

(law current-ns-var
  (= "app.core/f" (:fqn (resolve-name (sample-analysis) "app.core" "f"))))

(law alias-qualified
  (= "app.util/g" (:fqn (resolve-name (sample-analysis) "app.core" "u/g"))))

(law referred-name
  (= "app.util/g" (:fqn (resolve-name (sample-analysis) "app.core" "r"))))

(law core-name
  (= :core (:kind (resolve-name (sample-analysis) "app.core" "map"))))

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
