# h2

The HTTP2 (h2) protocol frames library can be used to pack and encode HTTP2 frames into a byte buffer, as well as to unpack and decode HTTP2 frames from a byte buffer.

## Installation

Simply add h2 as a dependency to your lein project:

```clojure
[h2 "0.1.0"]
```

## Examples

The functions pack, buffer, frame and unpack can be used with a key-value map representing an unpacked frame.

- Only packed frames can be converted to a buffer.
- Frames obtained from a buffer are packed and should be unpacked to remove padding and special value fields from the payload.

For more examples see the collections of unit tests.

```clojure
(ns my-namespace
  (:require [h2.protocol.frame :refer :all]))

; data frame example
(let [f {:type :data
         :stream 2
         :padding 66
         :flags #{:compressed :end-stream}
         :payload (b text)}]
  (-> f
      pack
      buffer
      frame
      unpack))
```

## License

Copyright © 2015 Jose Gomez

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
