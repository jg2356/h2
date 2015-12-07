(ns h2.hpack
  (:import [java.nio.charset Charset]))

(declare static-entry-table static-index-table)

(def charset
  (Charset/forName "ISO-8859-1"))

(defn b
  [s]
  (. s getBytes charset))

(defn s
  [b]
  (new String b 0 (count b) charset))

(def b-memo
  (memoize b))

(def s-memo
  (memoize s))

(defprotocol HasLength
  (length [this] "Returns the size of the data struture"))

(defrecord HeaderEntry [skey bkey sval bval]
  HasLength
  (length [this]
    (+ (-> this :bkey count)
       (-> this :bval count)
       32)))

(defn header-entry
  ([skey]
   (header-entry skey ""))
  ([skey sval]
   {:pre [skey sval]}
   (new HeaderEntry skey (b skey) sval (b sval))))

(defn dynamic-table
  [])

(defn get-static-entry
  [eidx]
  (get static-entry-table (dec eidx)))

(defn get-static-index
  ([skey]
   (-> (get static-index-table skey)
       (first)
       (last)))
  ([skey sval]
   (get-in static-index-table [skey sval])))

(def static-entry-table
  [(header-entry ":authority")                      ;  1
   (header-entry ":method" "GET")                   ;  2
   (header-entry ":method" "POST")                  ;  3
   (header-entry ":path" "/")                       ;  4
   (header-entry ":path" "/index.html")             ;  5
   (header-entry ":scheme" "http")                  ;  6
   (header-entry ":scheme" "https")                 ;  7
   (header-entry ":status" "200")                   ;  8
   (header-entry ":status" "204")                   ;  9
   (header-entry ":status" "206")                   ; 10
   (header-entry ":status" "304")                   ; 11
   (header-entry ":status" "400")                   ; 12
   (header-entry ":status" "404")                   ; 13
   (header-entry ":status" "500")                   ; 14
   (header-entry "accept-charset")                  ; 15
   (header-entry "accept-encoding" "gzip, deflate") ; 16
   (header-entry "accept-language")                 ; 17
   (header-entry "accept-ranges")                   ; 18
   (header-entry "accept")                          ; 19
   (header-entry "access-control-allow-origin")     ; 20
   (header-entry "age")                             ; 21
   (header-entry "allow")                           ; 22
   (header-entry "authorization")                   ; 23
   (header-entry "cache-control")                   ; 24
   (header-entry "content-disposition")             ; 25
   (header-entry "content-encoding")                ; 26
   (header-entry "content-language")                ; 27
   (header-entry "content-length")                  ; 28
   (header-entry "content-location")                ; 29
   (header-entry "content-range")                   ; 30
   (header-entry "content-type")                    ; 31
   (header-entry "cookie")                          ; 32
   (header-entry "date")                            ; 33
   (header-entry "etag")                            ; 34
   (header-entry "expect")                          ; 35
   (header-entry "expires")                         ; 36
   (header-entry "from")                            ; 37
   (header-entry "host")                            ; 38
   (header-entry "if-match")                        ; 39
   (header-entry "if-modified-since")               ; 40
   (header-entry "if-none-match")                   ; 41
   (header-entry "if-range")                        ; 42
   (header-entry "if-unmodified-since")             ; 43
   (header-entry "last-modified")                   ; 44
   (header-entry "link")                            ; 45
   (header-entry "location")                        ; 46
   (header-entry "max-forwards")                    ; 47
   (header-entry "proxy-authenticate")              ; 48
   (header-entry "proxy-authorization")             ; 49
   (header-entry "range")                           ; 50
   (header-entry "referer")                         ; 51
   (header-entry "refresh")                         ; 52
   (header-entry "retry-after")                     ; 53
   (header-entry "server")                          ; 54
   (header-entry "set-cookie")                      ; 55
   (header-entry "strict-transport-security")       ; 56
   (header-entry "transfer-encoding")               ; 57
   (header-entry "user-agent")                      ; 58
   (header-entry "vary")                            ; 59
   (header-entry "via")                             ; 60
   (header-entry "www-authenticate")])              ; 61

(def static-index-table
  (->> static-entry-table
       (map-indexed
         (fn [i e]
           [(inc i) e]))
       (group-by
         (comp :skey last))
       (map
         (fn [[skey iecoll]]
           [skey
            (->> iecoll
                 (map
                   (fn [[i e]]
                     [(:sval e) i]))
                 (apply concat)
                 (apply array-map))]))
       (apply concat)
       (apply array-map)))
