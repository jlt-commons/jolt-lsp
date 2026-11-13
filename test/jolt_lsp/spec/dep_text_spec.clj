(ns jolt-lsp.spec.dep-text-spec
  "Searching and reading dependency sources: the pure parts.

  A glob filters files by path, a library filter picks dependencies by
  lib name, the fallback search matches a regex line by line, a read
  windows a file by 1-based line, and a path is inside a dependency only
  when it sits under that dependency's directory."
  (:require [writ.spec :refer [spec ann graph law refine]]))

(spec jolt-lsp.dep-text {:require :tested})

(refine Match [m {:line Nat :text String}] true)

(refine Window [w {:content String
                   :start Nat
                   :end Nat
                   :total-lines Nat
                   :truncated Bool}] true)

(ann glob-matches? [(Opt String) String -> Bool])
(ann library-matches? [(Opt String) String -> Bool])
(ann grep-lines [String String Bool -> (List Match)])
(ann window [String Nat (Opt Nat) -> Window])
(ann inside? [String String -> Bool])
(ann clip [String Nat -> String])
(ann pattern-re [String Bool -> Any])

(graph searching
  {:start :text
   :states {:text String
            :matches (List Match)
            :window Window}
   :edges {:text {[grep-lines String Bool] #{:matches}
                  [window Nat (Opt Nat)] #{:window}}}
   :final [:matches :window]})

(law no-glob-matches-everything
  (forall [p String] (glob-matches? nil p)))

(law glob-extension (glob-matches? "*.clj" "src/a/core.clj"))
(law glob-other-extension (not (glob-matches? "*.clj" "src/a/core.cljc")))
(law glob-alternatives (glob-matches? "*.{clj,cljc}" "src/a/core.cljc"))
(law glob-dot-is-literal (not (glob-matches? "*.clj" "src/a/coreXclj")))

(law no-filter-matches-every-lib
  (forall [l String] (library-matches? nil l)))

(law filter-full-lib (library-matches? "acme/widget" "acme/widget"))
(law filter-group (library-matches? "acme" "acme/widget"))
(law filter-artifact (library-matches? "widget" "io.github.acme/widget"))
(law filter-other-lib (not (library-matches? "acme/gadget" "acme/widget")))

(law grep-finds-lines
  (= [{:line 2 :text "(defn spin [])"}]
     (grep-lines "(ns w)\n(defn spin [])\n" "defn\\s+spin" false)))

(law grep-case-insensitive
  (= [1] (map :line (grep-lines "Spin\n" "spin" true))))

(law grep-case-sensitive
  (empty? (grep-lines "Spin\n" "spin" false)))

(law grep-lines-are-in-range
  (forall [s String]
    (every? (fn [m] (<= 1 (:line m) (max 1 (count (re-seq #"\n" (str s "\n"))))))
            (grep-lines s "a" false))))

(law window-whole-file
  (let [w (window "a\nb\nc\n" 1 nil)]
    (and (= 1 (:start w)) (= 3 (:end w)) (= 3 (:total-lines w))
         (not (:truncated w)))))

(law window-slice
  (let [w (window "a\nb\nc\n" 2 1)]
    (and (= 2 (:start w)) (= 2 (:end w)) (:truncated w)
         (clojure.string/includes? (:content w) "b")
         (not (clojure.string/includes? (:content w) "c")))))

(law window-without-limit-runs-to-the-end
  (forall [s String, start Nat]
    (= (:end (window s start nil)) (count (clojure.string/split-lines s)))))

(law window-end
  (forall [s String, start Nat, limit Nat]
    (=> (>= start 1)
        (let [total (count (clojure.string/split-lines s))
              w (window s start limit)]
          (and (= start (:start w))
               (= total (:total-lines w))
               (= (:end w) (min total (max (dec start) (+ start limit -1))))
               (= (:truncated w) (< (:end w) total)))))))

(law window-numbers-its-lines
  (forall [s String, start Nat, limit Nat]
    (=> (>= start 1)
        (let [w (window s start limit)]
          (= (range start (inc (:end w)))
             (map #(parse-long (clojure.string/trim (first (clojure.string/split % #"\t" 2))))
                  (remove clojure.string/blank? (clojure.string/split (:content w) #"\n(?=\s*\d+\t)"))))))))

(law inside-a-dir (inside? "/deps/widget" "/deps/widget/src/w.clj"))
(law not-a-sibling (not (inside? "/deps/widget" "/deps/widget2/src/w.clj")))
(law not-the-dir-itself (not (inside? "/deps/widget" "/deps/widget")))
(law not-a-parent-escape (not (inside? "/deps/widget" "/deps/widget/../other/x.clj")))

(law inside-a-dir-with-trailing-slash (inside? "/deps/widget/" "/deps/widget/w.clj"))
(law inside-a-relative-dir (inside? "deps/widget" "deps/widget/./w.clj"))
(law relative-is-not-inside-absolute (not (inside? "/deps/widget" "deps/widget/w.clj")))
(law dotdot-above-root-stays-at-root (not (inside? "/deps" "/../etc/hosts")))

(law filter-lib-without-group (library-matches? "widget" "widget"))

(law clip-long-line (= "ab..." (clip "abcdef" 2)))
(law clip-short-line (= "ab" (clip "ab" 2)))

(law pattern-case-insensitive (some? (re-find (pattern-re "spin" true) "SPIN")))

(law window-zero-limit
  (let [w (window "a\nb\n" 1 0)]
    (and (= "" (:content w)) (= 0 (:end w)) (:truncated w))))

(law window-start-past-the-end
  (let [w (window "a\n" 5 nil)]
    (and (= "" (:content w)) (= 1 (:end w)) (not (:truncated w)))))

(law clip-keeps-short-cuts-long
  (forall [s String, n Nat]
    (= (clip s n) (if (> (count s) n) (str (subs s 0 n) "...") s))))

(law glob-star-matches-everything
  (forall [p String] (glob-matches? "*" p)))

(law glob-extension-exact
  (forall [n Nat, m Nat, k Nat]
    (= (glob-matches? (str "*.e" n) (str "dir/f" k ".e" m)) (= n m))))

(law grep-literal-lines
  (forall [s String]
    (= (map :line (grep-lines s "x" false))
       (keep-indexed (fn [i l] (when (clojure.string/includes? l "x") (inc i)))
                     (clojure.string/split-lines s)))))

(law grep-keeps-the-text
  (forall [s String]
    (every? (fn [m] (= (:text m) (nth (clojure.string/split-lines s) (dec (:line m)))))
            (grep-lines s "x" false))))

(law library-by-group
  (forall [n Nat, m Nat, k Nat]
    (= (library-matches? (str "g" k) (str "g" n "/a" m)) (= k n))))

(law library-by-artifact
  (forall [n Nat, m Nat, k Nat]
    (= (library-matches? (str "a" k) (str "g" n "/a" m)) (= k m))))

(law library-by-full-name
  (forall [n Nat, m Nat]
    (library-matches? (str "g" n "/a" m) (str "g" n "/a" m))))

(law inside-only-its-own-dir
  (forall [n Nat, m Nat, k Nat]
    (and (= (inside? (str "/d" n) (str "/d" m "/f" k)) (= n m))
         (= (inside? (str "/d" n "/") (str "/d" m "/f" k)) (= n m))
         (not (inside? (str "/d" n) (str "/d" n)))
         (not (inside? (str "/d" n) (str "/d" n "/../d" m "x/f" k))))))

(law inside-the-root (inside? "/" "/etc/hosts"))
(law the-root-is-not-inside-itself (not (inside? "/" "/")))

(law grep-any-literal
  (forall [s String, n Nat]
    (let [pat (str "p" n)]
      (= (map :line (grep-lines s pat false))
         (keep-indexed (fn [i l] (when (clojure.string/includes? l pat) (inc i)))
                       (clojure.string/split-lines s))))))
