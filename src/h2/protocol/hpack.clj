(ns h2.protocol.hpack
  (:require [clojure.java.io :refer [output-stream
                                     input-stream]])
  (:import [java.nio.charset Charset]
           [com.twitter.hpack Encoder Decoder HeaderListener]))

(def charset
  (Charset/forName "ISO-8859-1"))

(defn b
  [s]
  (. s getBytes charset))

(defn s
  [b]
  (new String b 0 (count b) charset))

(defn get-encoder
  ([] (make-encoder 4096))
  ([max-table-size]
   (let [enc (Encoder. max-table-size)]
     (fn [output ^String hkey ^String hval ^Boolean sensitive]
       (let [os (output-stream output)]
         (. enc encodeHeader os (b hkey) (b hval) sensitive)
         os)))))

(defn get-decoder
  ([] (make-decoder 4096 4096))
  ([max-header-size max-table-size]
   (let [dec (Decoder. max-header-size max-table-size)]
     (fn [input]
       (let [is (input-stream input)
             headers (atom [])
             listener (reify HeaderListener
                        (addHeader [this bkey bval sensitive]
                          (swap! headers conj [(s bkey) (s bval) sensitive])))]
         (. dec decode is listener)
         (. dec endHeaderBlock)
         @headers)))))
