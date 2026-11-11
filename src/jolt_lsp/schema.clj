(ns jolt-lsp.schema
  "Relatum DDL for the index. Loading this namespace registers the tables."
  (:require [relatum.schema :as s]))

(s/create-table :file
  [[:id :int :primary :key]
   [:path :text :required :unique]
   [:uri :text]
   [:ns :text :nullable]
   [:dialect :keyword]
   [:project :text]
   [:external :bool :default false]
   [:mtime :int :default 0]
   [:hash :text :nullable]])

(s/create-table :namespace
  [[:id :int :primary :key]
   [:file-id :int :required :references :file :on-delete :cascade]
   [:name :text :nullable]])

(s/create-table :ns-alias
  [[:id :int :primary :key :auto]
   [:ns-id :int :required :references :namespace :on-delete :cascade]
   [:alias :text :required]
   [:target :text :required]]
  :unique [[:ns-id :alias]])

(s/create-table :ns-require
  [[:id :int :primary :key :auto]
   [:ns-id :int :required :references :namespace :on-delete :cascade]
   [:target :text :required]
   [:alias :text :nullable]
   [:refer-all :bool :default false]
   [:as-alias :bool :default false]
   [:row :int] [:col :int] [:end-row :int] [:end-col :int]])

(s/create-table :ns-refer
  [[:id :int :primary :key :auto]
   [:ns-id :int :required :references :namespace :on-delete :cascade]
   [:name :text :required]
   [:target :text :required]])

(s/create-table :ns-import
  [[:id :int :primary :key :auto]
   [:ns-id :int :required :references :namespace :on-delete :cascade]
   [:simple :text :required]
   [:fq :text :required]])

(s/create-table :var-def
  [[:id :int :primary :key :auto]
   [:fqn :text :required :unique]
   [:ns :text :nullable]
   [:name :text :required]
   [:kind :keyword]
   [:file-id :int :required :references :file :on-delete :cascade]
   [:row :int] [:col :int] [:end-row :int] [:end-col :int]
   [:name-row :int] [:name-col :int] [:name-end-row :int] [:name-end-col :int]
   [:private :bool :default false]
   [:deprecated :bool :default false]
   [:doc :text :nullable]
   [:arglists :text :nullable]
   [:params :text :nullable]
   [:defined-by :text :nullable]])

(s/create-table :var-ref
  [[:id :int :primary :key :auto]
   [:file-id :int :required :references :file :on-delete :cascade]
   [:ns :text :nullable]
   [:text :text]
   [:name :text]
   [:prefix :text :nullable]
   [:fqn :text :nullable]
   [:qualified :bool :default false]
   [:in-call :bool :default false]
   [:caller :text :nullable]
   [:unresolved :bool :default false]
   [:row :int] [:col :int] [:end-row :int] [:end-col :int]
   [:name-row :int] [:name-col :int] [:name-end-row :int] [:name-end-col :int]])

(s/create-table :local-def
  [[:id :int :primary :key]
   [:file-id :int :required :references :file :on-delete :cascade]
   [:name :text :required]
   [:kind :keyword]
   [:row :int] [:col :int] [:end-row :int] [:end-col :int]
   [:scope-row :int] [:scope-col :int] [:scope-end-row :int] [:scope-end-col :int]
   [:keys-destructuring :bool :default false]
   [:destructured :bool :default false]])

(s/create-table :local-ref
  [[:id :int :primary :key :auto]
   [:local-id :int :required :references :local-def :on-delete :cascade]
   [:file-id :int :required :references :file :on-delete :cascade]
   [:name :text]
   [:row :int] [:col :int] [:end-row :int] [:end-col :int]])

(s/create-table :keyword-def
  [[:id :int :primary :key :auto]
   [:fqn :text :required :unique]
   [:file-id :int :required :references :file :on-delete :cascade]
   [:kind :keyword :default :keyword]
   [:row :int] [:col :int] [:end-row :int] [:end-col :int]])

(s/create-table :keyword-ref
  [[:id :int :primary :key :auto]
   [:fqn :text :required]
   [:file-id :int :required :references :file :on-delete :cascade]
   [:notation :keyword]
   [:row :int] [:col :int] [:end-row :int] [:end-col :int]])

(s/create-table :call-edge
  [[:id :int :primary :key :auto]
   [:caller :text :required]
   [:callee :text :required]
   [:file-id :int :required :references :file :on-delete :cascade]
   [:row :int] [:col :int] [:end-row :int] [:end-col :int]])

(s/create-table :ns-dep
  [[:id :int :primary :key :auto]
   [:from-ns :text :required]
   [:to-ns :text :required]
   [:kind :keyword :default :require]])

(s/create-table :diagnostic
  [[:id :int :primary :key :auto]
   [:file-id :int :required :references :file :on-delete :cascade]
   [:row :int] [:col :int] [:end-row :int] [:end-col :int]
   [:severity :keyword :required]
   [:code :keyword :required]
   [:message :text :required]
   [:source :keyword :default :jolt-lsp]])

(s/create-index :file [:ns] :type :hash)
(s/create-index :var-def [:ns] :type :hash)
(s/create-index :var-def [:kind] :type :hash)
(s/create-index :var-ref [:fqn] :type :hash)
(s/create-index :var-ref [:ns] :type :hash)
(s/create-index :local-def [:file-id :name] :type :btree)
(s/create-index :keyword-ref [:fqn] :type :hash)
(s/create-index :call-edge [:caller] :type :hash)
(s/create-index :call-edge [:callee] :type :hash)
(s/create-index :ns-dep [:from-ns] :type :hash)
(s/create-index :ns-dep [:to-ns] :type :hash)
(s/create-index :diagnostic [:code] :type :hash)
