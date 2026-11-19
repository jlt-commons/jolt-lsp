(ns jolt-lsp.types.utils
  "What the copied clj-kondo type specs call from their :fn entries.

  jolt-lsp reads a spec's :args and :ret only; the :fn entries, which
  compute a return type from the argument types, are never called. These
  keep the spec namespaces loadable and answer conservatively if they are."
  {:no-doc true})

(defn union-type
  "A type that is either a or b."
  [a b]
  (cond (= a b) a
        (or (= :any a) (= :any b)) :any
        :else (into (if (set? a) a #{a}) (if (set? b) b #{b}))))

(defn always-falsy? [t] (contains? #{:nil :false} t))
(defn never-falsy? [t] (not (contains? #{:nil :false :boolean :any} t)))
(defn falsy-part [_t] :any)
(defn truthy-part [_t] :any)
(defn fold-logic [_args _pred _part] :any)
