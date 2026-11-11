(ns jolt-lsp.lsp.rpc
  "LSP stdio framing: Content-Length headers, JSON bodies.

  Content-Length counts UTF-8 bytes. *in* yields decoded characters, so the
  body is read character by character until their UTF-8 length reaches the
  header's count; reading `n` characters would run into the next message
  whenever a body holds non-ASCII text."
  (:require [clojure.string :as string]
            [jolt-lsp.json :as json]
            [jolt-lsp.text :as text]))

(defn- utf8-length [cp]
  (cond (< cp 0x80) 1 (< cp 0x800) 2 (< cp 0x10000) 3 :else 4))

(defn- read-body [n]
  (let [sb (StringBuilder.)]
    (loop [bytes 0]
      (if (>= bytes n)
        (str sb)
        (let [c (.read *in*)]
          (if (neg? c)
            (str sb)
            (do (.append sb (char c))
                (recur (+ bytes (utf8-length c))))))))))

(defn read-message []
  (loop [headers {}]
    (let [line (read-line)]
      (cond
        (nil? line) nil
        (string/blank? line)
        (if-let [n (:content-length headers)]
          (json/read-json (read-body (parse-long n)))
          (recur headers))
        :else
        (let [[k v] (string/split line #":" 2)]
          (recur (assoc headers (keyword (string/lower-case (string/trim k)))
                        (string/trim (str v)))))))))

(defn write-message [m]
  (let [body (json/write m)
        n (text/utf8-offset body (count body))]
    (locking *out*
      (print (str "Content-Length: " n "\r\n\r\n" body))
      (flush))))

(defn notify [method params]
  (write-message {:jsonrpc "2.0" :method method :params params}))

(defonce ^:private request-id (atom 0))

(defn request
  "Send a request to the client. Its response is read and ignored by the
  server loop."
  [method params]
  (write-message {:jsonrpc "2.0" :id (str "jolt-lsp-" (swap! request-id inc))
                  :method method :params params}))

(defn respond [id result]
  (write-message {:jsonrpc "2.0" :id id :result result}))

(defn respond-error [id code message]
  (write-message {:jsonrpc "2.0" :id id :error {:code code :message message}}))
