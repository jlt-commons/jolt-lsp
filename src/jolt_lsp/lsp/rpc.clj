(ns jolt-lsp.lsp.rpc
  "LSP stdio framing: Content-Length headers, JSON bodies.

  Frames are read as bytes from System/in. On Jolt, `.read` on `*in*` waits
  for a whole line or EOF (https://github.com/burinc/jolt/issues/22) and an
  LSP body has no newline, so a character reader leaves a request unanswered
  until more input arrives. `.read` on System/in returns what is there.
  Content-Length counts UTF-8 bytes, so reading bytes makes it exact for
  non-ASCII bodies. Nothing else in lsp mode reads *in*, since two readers
  would fight over buffered input."
  (:require [clojure.string :as string]
            [jolt-lsp.json :as json]
            [jolt-lsp.text :as text]))

(def ^:dynamic *in-stream*
  "The byte stream frames are read from. Bound in tests."
  System/in)

(defn- read-header-line
  "One header line as a string without its line end, or nil at end of input
  with nothing read."
  [in]
  (let [buf (java.io.ByteArrayOutputStream.)]
    (loop [seen? false]
      (let [b (.read in)]
        (cond
          (neg? b) (when seen? (.toString buf "UTF-8"))
          (= b 10) (string/replace (.toString buf "UTF-8") #"\r$" "")
          :else (do (.write buf b) (recur true)))))))

(defn- read-body
  "Exactly n bytes decoded as UTF-8, looping on short reads. Shorter only
  when the input ends first."
  [in n]
  (let [buf (byte-array n)]
    (loop [off 0]
      (if (>= off n)
        (String. buf "UTF-8")
        (let [r (.read in buf off (- n off))]
          (if (neg? r)
            (String. (java.util.Arrays/copyOf buf off) "UTF-8")
            (recur (+ off r))))))))

(defn read-message []
  (let [in *in-stream*]
    (loop [headers {}]
      (let [line (read-header-line in)]
        (cond
          (nil? line) nil
          (string/blank? line)
          (if-let [n (:content-length headers)]
            (json/read-json (read-body in (parse-long n)))
            (recur headers))
          :else
          (let [[k v] (string/split line #":" 2)]
            (recur (assoc headers (keyword (string/lower-case (string/trim k)))
                          (string/trim (str v))))))))))

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
