(ns jolt-lsp.lsp.rpc
  "LSP stdio framing: Content-Length headers, JSON bodies."
  (:require [clojure.string :as string]
            [jolt-lsp.json :as json]
            [jolt-lsp.text :as text]))

(defn- read-body [n]
  (apply str (repeatedly n #(let [c (.read *in*)]
                              (if (neg? c) \space (char c))))))

(defn read-message []
  (loop [headers {}]
    (let [line (read-line)]
      (cond
        (nil? line) nil
        (string/blank? line)
        (when-let [n (:content-length headers)]
          (json/read-json (read-body (read-string n))))
        :else
        (let [[k v] (string/split line #":" 2)]
          (recur (assoc headers (keyword (string/lower-case (string/trim k)))
                        (string/trim v))))))))

(defn write-message [m]
  (let [body (json/write m)
        n (text/utf8-offset body (count body))]
    (print (str "Content-Length: " n "\r\n\r\n" body))
    (flush)))

(defn notify [method params]
  (write-message {:jsonrpc "2.0" :method method :params params}))

(defn respond [id result]
  (write-message {:jsonrpc "2.0" :id id :result result}))

(defn respond-error [id code message]
  (write-message {:jsonrpc "2.0" :id id :error {:code code :message message}}))
