(ns jolt-lsp.spec.extract-spec
  "What a file declares: namespace, vars, aliases, refers, requires, keywords.

  Comment, quote and #_ discard forms define nothing."
  (:require [writ.spec :refer [spec ann graph law]]))

(spec jolt-lsp.extract {:require :tested})

(ann ns-name [String String -> (Opt String)])
(ann var-names [String String -> (List String)])
(ann private-var-names [String String -> (List String)])
(ann alias-map [String String -> (Map String String)])
(ann refer-map [String String -> (Map String String)])
(ann required-namespaces [String String -> (Set String)])
(ann keyword-fqns [String String -> (Vec String)])

(graph extracting
  {:start :source
   :states {:source String
            :ns (Opt String)
            :names (List String)
            :keywords (Vec String)
            :aliases (Map String String)
            :requires (Set String)}
   :edges {:source {[ns-name String] #{:ns}
                    [var-names String] #{:names}
                    [private-var-names String] #{:names}
                    [alias-map String] #{:aliases}
                    [refer-map String] #{:aliases}
                    [required-namespaces String] #{:requires}
                    [keyword-fqns String] #{:keywords}}}
   :final [:ns :names :keywords :aliases :requires]})

(law no-ns
  (nil? (ns-name "" "a.clj")))

(law ns-name-found
  (= "app.core" (ns-name "(ns app.core)" "core.clj")))

(law no-vars-in-empty
  (empty? (var-names "" "a.clj")))

(law defn-found
  (= ["f"] (var-names "(ns a)\n(defn f [x] x)" "a.clj")))

(law defn-private
  (= ["p"] (private-var-names "(ns a)\n(defn- p [] 1)" "a.clj")))

(law comment-ignored
  (empty? (var-names "(ns a)\n(comment (def x 1))" "a.clj")))

(law quote-ignored
  (empty? (var-names "(ns a)\n'(def x 1)" "a.clj")))

(law discard-ignored
  (empty? (var-names "(ns a)\n#_(def x 1)" "a.clj")))

(law alias-found
  (= {"string" "clojure.string"}
     (alias-map "(ns a (:require [clojure.string :as string]))" "a.clj")))

(law refer-found
  (= {"join" "clojure.string/join"}
     (refer-map "(ns a (:require [clojure.string :refer [join]]))" "a.clj")))

(law require-found
  (= #{"clojure.set"}
     (required-namespaces "(ns a (:require [clojure.set]))" "a.clj")))

(law qualified-keyword
  (= [":app.core/db"]
     (keyword-fqns "(ns app.core)\n(def x :app.core/db)" "core.clj")))

(law plain-keyword
  (= [":db"]
     (keyword-fqns "(ns a)\n(def x :db)" "a.clj")))

(law empty-alias-map (= {} (alias-map "" "a.clj")))
(law empty-refer-map (= {} (refer-map "" "a.clj")))
(law empty-requires (= #{} (required-namespaces "" "a.clj")))
(law empty-keywords (= [] (keyword-fqns "" "a.clj")))

(law var-names-sees-prefix
  (forall [s String]
    (some (fn [n] (= n "zz"))
          (var-names (str "(defn zz [] 1)\n" s) "a.clj"))))

(law private-var-names-prefix
  (forall [s String]
    (some (fn [n] (= n "pp"))
          (private-var-names (str "(ns a)\n(defn- pp [] 1)\n" s) "a.clj"))))

(law ns-name-prefix
  (forall [s String]
    (= (ns-name (str "(ns zz)\n" s) "a.clj") "zz")))

(law alias-map-prefix
  (forall [s String]
    (= "clojure.string"
       (get (alias-map (str "(ns a (:require [clojure.string :as str]))\n" s) "a.clj")
            "str"))))

(law refer-map-prefix
  (forall [s String]
    (= "clojure.string/join"
       (get (refer-map (str "(ns a (:require [clojure.string :refer [join]]))\n" s) "a.clj")
            "join"))))

(law requires-prefix
  (forall [s String]
    (contains? (required-namespaces (str "(ns a (:require [clojure.set]))\n" s) "a.clj")
               "clojure.set")))

(law keyword-prefix
  (forall [s String]
    (some (fn [k] (= k ":a/x"))
          (keyword-fqns (str "(ns a)\n(def x :a/x)\n" s) "a.clj"))))

(law path-independent-var-names
  (forall [s String, p String] (= (var-names s p) (var-names s "a.clj"))))

(law path-independent-private-var-names
  (forall [s String, p String] (= (private-var-names s p) (private-var-names s "a.clj"))))

(law path-independent-ns-name
  (forall [s String, p String] (= (ns-name s p) (ns-name s "a.clj"))))

(law path-independent-alias-map
  (forall [s String, p String] (= (alias-map s p) (alias-map s "a.clj"))))

(law path-independent-refer-map
  (forall [s String, p String] (= (refer-map s p) (refer-map s "a.clj"))))

(law path-independent-requires
  (forall [s String, p String] (= (required-namespaces s p) (required-namespaces s "a.clj"))))

(law path-independent-keywords
  (forall [s String, p String] (= (keyword-fqns s p) (keyword-fqns s "a.clj"))))

(law keyword-order
  (= (keyword-fqns "(ns a)\n(def x :a/x)\n(def y :a/y)" "a.clj")
     [":a/x" ":a/y"]))
