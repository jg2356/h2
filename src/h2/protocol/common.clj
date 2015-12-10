(ns h2.protocol.common
  (:import [java.nio.charset Charset]))

(def charset
  (Charset/forName "ISO-8859-1"))

(defn b
  "Converts a string to bytes"
  [s]
  (. s getBytes charset))

(defn s
  "Converts bytes to a string"
  [b]
  (new String b 0 (count b) charset))
