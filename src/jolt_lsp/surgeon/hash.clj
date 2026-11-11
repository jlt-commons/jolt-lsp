(ns jolt-lsp.surgeon.hash
  "Content revisions for the surgeon plans: the SHA-256 of UTF-8 text, as
  lowercase hex. Pure Clojure (FIPS 180-4), so jolt-lsp needs no crypto
  provider for it; the digest is the standard one, so a revision computed
  here equals one computed by any other SHA-256.")

(def ^:private k
  [0x428a2f98 0x71374491 0xb5c0fbcf 0xe9b5dba5 0x3956c25b 0x59f111f1 0x923f82a4 0xab1c5ed5
   0xd807aa98 0x12835b01 0x243185be 0x550c7dc3 0x72be5d74 0x80deb1fe 0x9bdc06a7 0xc19bf174
   0xe49b69c1 0xefbe4786 0x0fc19dc6 0x240ca1cc 0x2de92c6f 0x4a7484aa 0x5cb0a9dc 0x76f988da
   0x983e5152 0xa831c66d 0xb00327c8 0xbf597fc7 0xc6e00bf3 0xd5a79147 0x06ca6351 0x14292967
   0x27b70a85 0x2e1b2138 0x4d2c6dfc 0x53380d13 0x650a7354 0x766a0abb 0x81c2c92e 0x92722c85
   0xa2bfe8a1 0xa81a664b 0xc24b8b70 0xc76c51a3 0xd192e819 0xd6990624 0xf40e3585 0x106aa070
   0x19a4c116 0x1e376c08 0x2748774c 0x34b0bcb5 0x391c0cb3 0x4ed8aa4a 0x5b9cca4f 0x682e6ff3
   0x748f82ee 0x78a5636f 0x84c87814 0x8cc70208 0x90befffa 0xa4506ceb 0xbef9a3f7 0xc67178f2])

(def ^:private h0
  [0x6a09e667 0xbb67ae85 0x3c6ef372 0xa54ff53a 0x510e527f 0x9b05688c 0x1f83d9ab 0x5be0cd19])

(defn- u32 [x] (bit-and x 0xffffffff))

(defn- rotr [x n]
  (u32 (bit-or (unsigned-bit-shift-right x n) (bit-shift-left x (- 32 n)))))

(defn- utf8-bytes
  "The UTF-8 encoding of `s` as longs 0..255. Jolt strings are sequences of
  codepoints, so each character is one codepoint."
  [s]
  (persistent!
   (reduce (fn [out ch]
             (let [cp (int ch)]
               (cond
                 (< cp 0x80) (conj! out cp)
                 (< cp 0x800) (-> out (conj! (bit-or 0xc0 (bit-shift-right cp 6)))
                                  (conj! (bit-or 0x80 (bit-and cp 0x3f))))
                 (< cp 0x10000) (-> out (conj! (bit-or 0xe0 (bit-shift-right cp 12)))
                                    (conj! (bit-or 0x80 (bit-and (bit-shift-right cp 6) 0x3f)))
                                    (conj! (bit-or 0x80 (bit-and cp 0x3f))))
                 :else (-> out (conj! (bit-or 0xf0 (bit-shift-right cp 18)))
                           (conj! (bit-or 0x80 (bit-and (bit-shift-right cp 12) 0x3f)))
                           (conj! (bit-or 0x80 (bit-and (bit-shift-right cp 6) 0x3f)))
                           (conj! (bit-or 0x80 (bit-and cp 0x3f)))))))
           (transient []) (str s))))

(defn- padded
  "Message bytes padded to a multiple of 64, with the 64-bit bit length."
  [bytes]
  (let [len (count bytes)
        bits (* 8 len)
        zeros (mod (- 55 len) 64)]
    (persistent!
     (as-> (conj! (transient bytes) 0x80) v
       (reduce (fn [v _] (conj! v 0)) v (range zeros))
       (reduce (fn [v i] (conj! v (bit-and (unsigned-bit-shift-right bits (* 8 (- 7 i))) 0xff)))
               v (range 8))))))

(defn- block-words [bs off]
  (let [w (transient (vec (for [i (range 16)
                                :let [o (+ off (* 4 i))]]
                            (bit-or (bit-shift-left (nth bs o) 24)
                                    (bit-shift-left (nth bs (+ o 1)) 16)
                                    (bit-shift-left (nth bs (+ o 2)) 8)
                                    (nth bs (+ o 3))))))]
    (loop [i 16 w w]
      (if (= i 64)
        (persistent! w)
        (let [w15 (nth w (- i 15)) w2 (nth w (- i 2))
              s0 (bit-xor (rotr w15 7) (rotr w15 18) (unsigned-bit-shift-right w15 3))
              s1 (bit-xor (rotr w2 17) (rotr w2 19) (unsigned-bit-shift-right w2 10))]
          (recur (inc i) (conj! w (u32 (+ (nth w (- i 16)) s0 (nth w (- i 7)) s1)))))))))

(defn- compress [hs w]
  (loop [i 0 a (hs 0) b (hs 1) c (hs 2) d (hs 3) e (hs 4) f (hs 5) g (hs 6) h (hs 7)]
    (if (= i 64)
      (mapv (fn [x y] (u32 (+ x y))) hs [a b c d e f g h])
      (let [s1 (bit-xor (rotr e 6) (rotr e 11) (rotr e 25))
            ch (bit-xor (bit-and e f) (bit-and (bit-xor e 0xffffffff) g))
            t1 (u32 (+ h s1 ch (k i) (w i)))
            s0 (bit-xor (rotr a 2) (rotr a 13) (rotr a 22))
            maj (bit-xor (bit-and a b) (bit-and a c) (bit-and b c))
            t2 (u32 (+ s0 maj))]
        (recur (inc i) (u32 (+ t1 t2)) a b c (u32 (+ d t1)) e f g)))))

(defn sha256-hex
  "The SHA-256 of string `s`'s UTF-8 bytes, as 64 lowercase hex digits."
  [s]
  (let [bs (padded (utf8-bytes (str s)))
        hs (reduce (fn [hs off] (compress hs (block-words bs off)))
                   h0 (range 0 (count bs) 64))]
    (apply str (map #(let [h (Long/toHexString %)]
                       (str (subs "00000000" (count h)) h))
                    hs))))

(defn content-revision
  "The SHA-256 revision of UTF-8 text, or nil for absent content."
  [text]
  (when (some? text)
    (sha256-hex text)))
