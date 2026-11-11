(ns jolt-lsp.read
  "Safe reading. Total: a broken buffer is a value, never an exception."
  (:require [jolt-lsp.read-impl :as impl]))

(defn parse
  "The parse result: {:ok bool :root node :forms vector :count n :error msg}."
  [text]
  (impl/parse-text text))

(defn valid?
  [text]
  (:ok (impl/parse-text text)))

(defn error-message
  [text]
  (:error (impl/parse-text text)))

(defn top-level-count
  [text]
  (:count (impl/parse-text text)))
