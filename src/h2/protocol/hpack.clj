(ns h2.protocol.hpack
  (:require [clojure.java.io :refer [output-stream input-stream]]
            [h2.protocol.common :as c])
  (:import [com.twitter.hpack Encoder Decoder HeaderListener]))

(defn get-encoder
  "Creates an hpack encoder function"
  ([] (get-encoder 4096))
  ([max-table-size]
   (let [enc (Encoder. max-table-size)]
     (fn [output ^String hkey ^String hval ^Boolean sensitive]
       (let [os (output-stream output)]
         (. enc encodeHeader os (c/b hkey) (c/b hval) sensitive)
         os)))))

(defn get-decoder
  "Creates an hpack decoder function"
  ([] (get-decoder 4096 4096))
  ([max-header-size max-table-size]
   (let [dec (Decoder. max-header-size max-table-size)]
     (fn [input]
       (let [is (input-stream input)
             headers (atom [])
             listener (reify HeaderListener
                        (addHeader [this bkey bval sensitive]
                          (swap! headers conj [(c/s bkey) (c/s bval) sensitive])))]
         (. dec decode is listener)
         (. dec endHeaderBlock)
         @headers)))))
