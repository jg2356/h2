# h2

The HTTP2 (h2) protocol frames library can be used to pack and encode HTTP2 frames into a byte buffer, as well as to unpack and decode HTTP2 frames from a byte buffer.

## Installation

Simply add h2 as a dependency to your lein project:

[![Clojars Project](http://clojars.org/h2/latest-version.svg)](http://clojars.org/h2)

[![Build Status](https://travis-ci.org/jg2356/h2.svg)](https://travis-ci.org/jg2356/h2)

## Examples

The functions pack, buffer, frame and unpack can be used with a key-value map representing an unpacked frame.

- Only packed frames can be converted to a buffer.
- Frames obtained from a buffer are packed and should be unpacked to remove padding and special value fields from the payload.

For more examples see the collections of unit tests.

```clojure
(ns my-namespace
  (:require [h2.protocol.frame :refer :all]
            [h2.protocol.common :refer :all]))

; data frame example
(let [f {:type :data
         :stream 2
         :padding 66
         :flags #{:compressed :end-stream}
         :payload (b "this is a test")}]
  (-> f
      pack
      buffer
      frame
      unpack))
; returns: {:length 14, :type :data, :flags #{:end-stream :compressed}, :stream 2, :padding 66, :payload ...}
```

The following example starts up a server and client and sends/receives HTTP2 packets on both ends after receiving the connection preface on the server.

```clojure
(use 'h2.protocol.common 'h2.protocol.tcp)

(def myserver
  (server
    :host "127.0.0.1"
    :port 13370
    :handler (fn [conn frame]
               (println "server received frame: " frame " with payload: " (-> frame :payload s))
               (transmit-frame conn {:type :data
                                     :stream 10
                                     :padding 50
                                     :payload (b "Hello from server")}))))

(def myclient
  (client :host "127.0.0.1"
          :port 13370
          :handler (fn [conn frame]
                     (println "client received frame: " frame " with payload: " (-> frame :payload s)))))

(start-server myserver)
(start-client myclient)

(transmit-frame myclient {:type :data
                          :stream 9
                          :padding 65
                          :payload (b "Hello from client")})
;;;server received frame:  {:length 17, :type :data, :flags #{}, :stream 9, :payload #object[[B 0x52900a6 [B@52900a6], :padding 65} with payload:  Hello from client
;;;client received frame:  {:length 17, :type :data, :flags #{}, :stream 10, :payload #object[[B 0x618edbfe [B@618edbfe], :padding 50} with payload:  Hello from server
```

## License

Copyright © 2015 Jose Gomez

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
