(ns h2.protocol.spec
  (:require [h2.protocol.common :refer :all]
            [clojure.math.numeric-tower :as math]
            [clojure.set :refer [map-invert]]))

(def h2-preface
  "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n")

(def h2-preface-bytes
  (b h2-preface))

(def h2-preface-length
  (count h2-preface-bytes))

(def h2-header-length 9)

(def max-stream-id
  (math/expt 2 31))

(def max-windowinc
  (math/expt 2 31))

(declare frame-type-to-code-map
         frame-code-to-type-map
         frame-type-to-flag-index-map
         frame-type-to-index-flag-map
         setting-type-to-code-map
         setting-code-to-type-map
         setting-type-to-initial-value-map
         error-type-to-code-map
         error-code-to-type-map)

(defn frame-code
  "Get the frame code"
  [type]
  (get frame-type-to-code-map type))

(defn frame-type
  "Get the frame type"
  [code]
  (get frame-code-to-type-map code))

(defn frame-flag-index
  "Get the frame flag token to flag index lookup"
  [type]
  (get frame-type-to-flag-index-map type))

(defn frame-padding?
  "Does the frame type allow padding?"
  [type]
  (-> type frame-flag-index :padded nil? not))

(defn frame-priority?
  "Does the frame type allow priority?"
  [type]
  (or (-> type frame-flag-index :priority nil? not)
      (= type :priority)))

(defn frame-index-flag
  "Get the frame flag index to flag token lookup"
  [type]
  (get frame-type-to-index-flag-map type))

(defn setting-code
  "Get the setting code"
  [type]
  (get setting-type-to-code-map type))

(defn setting-type
  "Get the setting type"
  [code]
  (get setting-code-to-type-map code))

(defn setting-init
  "Get the setting initial value"
  ([]
   setting-type-to-initial-value-map)
  ([type]
   (get setting-type-to-initial-value-map type)))

(defn error-code
  "Get the error code"
  [type]
  (get error-type-to-code-map type))

(defn error-type
  "Get the error type"
  [code]
  (get error-code-to-type-map code))

(def frame-type-to-code-map
  {:data             0x0
   :headers          0x1
   :priority         0x2
   :rst-stream       0x3
   :settings         0x4
   :push-promise     0x5
   :ping             0x6
   :goaway           0x7
   :window-update    0x8
   :continuation     0x9})

(def frame-code-to-type-map
  (map-invert frame-type-to-code-map))

(def frame-type-to-flag-index-map
  {:data             {:end-stream   0
                      :padded       3
                      :compressed   5}
   :headers          {:end-stream   0
                      :end-headers  2
                      :padded       3
                      :priority     5}
   :priority         {}
   :rst-stream       {}
   :settings         {:ack          0}
   :push-promise     {:end-headers  2
                      :padded       3}
   :ping             {:ack          0}
   :goaway           {}
   :window-update    {}
   :continuation     {:end-headers  2}})

(def frame-type-to-index-flag-map
  (into {} (for [[type flags] frame-type-to-flag-index-map]
             [type (map-invert flags)])))

(def setting-type-to-code-map
  {:settings-header-table-size       0x1
   :settings-enable-push             0x2
   :settings-max-concurrent-streams  0x3
   :settings-initial-window-size     0x4
   :settings-max-frame-size          0x5
   :settings-max-header-list-size    0x6})

(def setting-code-to-type-map
  (map-invert setting-type-to-code-map))

(def setting-type-to-initial-value-map
  {:settings-header-table-size       4096
   :settings-enable-push             1
   :settings-max-concurrent-streams  ::unlimited
   :settings-initial-window-size     65535
   :settings-max-frame-size          (math/expt 2 14)
   :settings-max-header-list-size    ::unlimited})

(def error-type-to-code-map
  {:no-error            0x0
   :protocol-error      0x1
   :internal-error      0x2
   :flow-control-error  0x3
   :settings-timeout    0x4
   :stream-closed       0x5
   :frame-size-error    0x6
   :refused-stream      0x7
   :cancel              0x8
   :compression-error   0x9
   :connect-error       0xa
   :enhance-your-calm   0xb
   :inadequate-security 0xc
   :http-1-1-required   0xd})

(def error-code-to-type-map
  (map-invert error-type-to-code-map))
