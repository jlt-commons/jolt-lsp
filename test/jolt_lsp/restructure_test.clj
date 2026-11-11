(ns jolt-lsp.restructure-test
  "The project-level operations moved here from samizdat: structural diff,
  namespace rename, declare cleanup and extraction, through jolt-lsp.api."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jolt-lsp.api :as api]
            [jolt-lsp.json :as json]
            [jolt-lsp.mcp :as mcp]
            [jolt-lsp.surgeon.hash :as hash]))

(defn- scratch []
  (let [dir (str (fs/create-temp-dir {:prefix "jolt-lsp-restructure"}))]
    (fs/copy-tree "test/fixtures/restructure" dir)
    (str (fs/canonicalize dir))))

(deftest content-revision-is-standard-sha256
  (is (= "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" (hash/content-revision "")))
  (is (= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" (hash/content-revision "abc")))
  (is (= "76f361755e75d092f4f6495aefdfe9369d1a3b53bf2f6d8b2b9aad2dd337b143"
         (hash/content-revision (apply str (repeat 100 "héllo ☃ 𝄞")))))
  (is (nil? (hash/content-revision nil))))

(deftest structural-diff-tells-a-rename-from-a-behaviour-change
  (let [root (scratch)
        r (api/structural-diff root {:file "src/shop/core.clj"
                                     :old "(ns a)\n(defn f [x] (inc x))\n(defn g [] 1)\n"
                                     :new "(ns a)\n(defn f [x] (dec x))\n;; g\n(defn g [] 1)\n"})]
    (is (:ok r))
    (is (= :semantic (:verdict r)))
    (is (= ["defn f"] (mapv #(str/join " " (map str (:id %))) (:forms r))))
    (is (str/includes? (:text r) "defn f")))
  (testing "a comment-only edit is not a behaviour change"
    (let [r (api/structural-diff (scratch) {:file "x.clj" :old "(ns a)\n(defn f [] 1)\n"
                                            :new "(ns a)\n;; one\n(defn f [] 1)\n"})]
      (is (not= :semantic (:verdict r))))))

(deftest rename-namespace-moves-the-file-and-its-callers
  (let [root (scratch)
        preview (api/rename-namespace* root {:from "shop.core" :to "shop.pricing"})]
    (is (:ok preview) (pr-str preview))
    (is (false? (:applied preview)))
    (is (fs/exists? (str root "/src/shop/core.clj")) "a preview writes nothing")
    (is (nil? (:texts preview)))
    (is (str/includes? (get (:texts (api/rename-namespace* root {:from "shop.core" :to "shop.pricing" :texts true}))
                            "src/shop/pricing.clj")
                       "(ns shop.pricing")
        ":texts gives a library caller the new text")
    (let [r (api/rename-namespace* root {:from "shop.core" :to "shop.pricing" :apply true})]
      (is (:applied r))
      (is (= ["src/shop/core.clj"] (:deleted r)))
      (is (not (fs/exists? (str root "/src/shop/core.clj"))))
      (is (str/includes? (slurp (str root "/src/shop/pricing.clj")) "(ns shop.pricing"))
      (is (str/includes? (slurp (str root "/src/shop/web.clj")) "[shop.pricing :as core]")))))

(deftest fix-declares-moves-the-definition-above-its-use
  (let [root (scratch)
        r (api/fix-declares* root {:file "src/shop/core.clj" :apply true})
        text (slurp (str root "/src/shop/core.clj"))]
    (is (:ok r) (pr-str r))
    (is (not (str/includes? text "(declare total)")))
    (is (< (str/index-of text "(defn total") (str/index-of text "(defn receipt")))))

(deftest extract-moves-forms-to-a-new-namespace
  (let [root (scratch)
        r (api/extract* root {:file "src/shop/core.clj" :forms ["label"] :to-ns "shop.labels"
                              :apply true})]
    (is (:ok r) (pr-str r))
    (is (= "src/shop/labels.clj" (:target r)))
    (is (= ["src/shop/web.clj"] (:callers r)) "the index knows web.clj calls core/label")
    (let [labels (slurp (str root "/src/shop/labels.clj"))
          core (slurp (str root "/src/shop/core.clj"))]
      (is (str/includes? labels "(ns shop.labels"))
      (is (str/includes? labels "(defn label"))
      (is (not (str/includes? core "(defn label"))))))

(deftest the-new-operations-are-mcp-tools-with-json-results
  (let [root (scratch)]
    (doseq [[tool args] [["jolt_structural_diff" {:root root :file "src/shop/core.clj"
                                                  :old "(ns shop.core)\n(defn f [] 1)\n"}]
                         ["jolt_rename_namespace" {:root root :from "shop.web" :to "shop.site"}]
                         ["jolt_fix_declares" {:root root :file "src/shop/core.clj"}]
                         ["jolt_extract" {:root root :file "src/shop/core.clj" :forms ["label"]
                                          :to_ns "shop.labels"}]]]
      (let [r (mcp/call-tool tool args)]
        (is (:ok r) (str tool " " (pr-str r)))
        (is (string? (json/write r)) tool)))
    (is (fs/exists? (str root "/src/shop/web.clj")) "previews only")))
