# h2

The HTTP2 (h2) protocol frames library can be used to pack and encode HTTP2 frames into a byte buffer, as well as to unpack and decode HTTP2 frames from a byte buffer.

## Installation

Simply add h2 as a dependency to your lein project:

```clojure
[h2 "0.1.0"]
```

## Examples

```clojure
(ns my-namespace
  (:require [h2.protocol.frame :refer :all]))

; data frame example
(let [frame {:type :data
             :stream 2
             :padding 66
             :flags #{:compressed :end-stream}
             :payload (b text)}
      packed (pack frame)]
  (buffer packed))

```

### Bugs

...

## License

Copyright © 2015 Jose Gomez

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
