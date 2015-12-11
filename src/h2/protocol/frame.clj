(ns h2.protocol.frame
  (:require [clojure.set]
            [h2.protocol.common :refer :all]
            [h2.protocol.error :refer [raise]]
            [h2.protocol.spec :as spec]
            [clojurewerkz.buffy.util :refer :all]
            [clojurewerkz.buffy.core :refer :all]
            [clojurewerkz.buffy.types.protocols :refer :all]
            [clojurewerkz.buffy.frames :refer :all]))

(def exclusive-bit 0x80000000)

(def reserved-bit 0x7fffffff)

(defn- validate-priority
  "Validates frame priority"
  [{:keys [type weight stream-dependency exclusive]
    :as frame}
   & _ ]
  (when-not (and weight stream-dependency (-> exclusive nil? not))
    (raise :compression-error "Must specify all of priority parameters for: "type))
  frame)

(defn- encode-priority
  "Encode frame priority"
  [{:keys [type length payload flags weight stream-dependency exclusive]
    :as frame}
   & _]
  {:pre [frame]}
  (let [frame (validate-priority frame)
        flags (cond-> flags (not= type :priority) (conj :priority))
        exval (if exclusive exclusive-bit 0)
        sdval (bit-and stream-dependency reserved-bit)
        prval (bit-or exval sdval)
        weval (dec weight)
        prbuf (-> (spec :priority (int32-type)
                        :weight (byte-type))
                  (compose-buffer)
                  (set-field :priority prval)
                  (set-field :weight weval)
                  (buffer))
        pbytes (read (bytes-type 5) prbuf 0)
        payload (concat pbytes payload)
        length (count payload)]
    (-> frame
        (merge
         {:length length
          :flags flags
          :payload (byte-array payload)}))))

(defn- decode-priority
  "Decode frame priority"
  [{:keys [payload length]
    :as frame}
   & _]
  {:pre [frame]}
  (let [length (- length 5)
        prbuf (-> (spec :priority (int32-type)
                        :weight (byte-type)
                        :payload (bytes-type length))
                  (compose-buffer :orig-buffer payload))
        prval (get-field prbuf :priority)
        sdval (bit-and prval reserved-bit)
        exval (bit-and prval exclusive-bit)
        weval (get-field prbuf :weight)
        payload (get-field prbuf :payload)]
    (-> frame
        (merge
          {:length length
           :stream-dependency sdval
           :exclusive (not= exval 0)
           :weight (inc weval)
           :payload payload}))))

(defn- validate-error
  "Validates error codes"
  [{:keys [error]
    :as frame}
   & _]
  {:pre [error]}
  (when-not (spec/error-code error)
    (raise :compression-error "Unknown error type: " error))
  frame)

(defn- encode-error
  "Encode frame error"
  [{:keys [error payload]
    :as frame}
   & _]
  {:pre [frame]}
  (let [erval (spec/error-code error)
        erbuf (-> (spec :error (int32-type))
                  (compose-buffer)
                  (set-field :error erval)
                  (buffer))
        ebytes (read (bytes-type 4) erbuf 0)
        payload (concat ebytes payload)]
    (-> frame
        (validate-error)
        (merge
          {:length (count payload)
           :payload (byte-array payload)}))))

(defn- decode-error
  "Decode frame error"
  [{:keys [payload length]
    :as frame}
   & _]
  {:pre [frame]}
  (let [length (- length 4)
        erbuf (-> (spec :error (int32-type)
                        :payload (bytes-type length))
                  (compose-buffer :orig-buffer payload))
        erval (get-field erbuf :error)
        error (or (spec/error-type erval) :internal-error)
        payload (get-field erbuf :payload)]
    (-> frame
        (merge
          {:error error
           :length length
           :payload payload}))))

(defn- validate-padding
  "Validates frame padding"
  [{:keys [type length padding]
    :as frame}
   {:keys [settings-max-frame-size]}]
  {:pre [type length padding settings-max-frame-size]}
  (when-not (spec/frame-padding? type)
    (raise :compression-error "Invalid padding flag for: " type))
  (when (or (<= padding 0) (> padding 256) (> (+ padding length) settings-max-frame-size))
    (raise :compression-error "Invalid padding: " padding))
  frame)

(defn- encode-padding
  "Encode frame padding"
  [{:keys [payload type length padding flags]
    :as frame}
   settings]
  {:pre [frame settings]}
  (let [padlen (byte (dec padding))
        flags (conj flags :padded)
        payload (concat [padlen] payload (byte-array padlen))
        length (count payload)]
    (-> frame
        (validate-padding settings)
        (dissoc :padding)
        (merge
         {:length length
          :flags flags
          :payload (byte-array payload)}))))

(defn- decode-padding
  "Decode frame padding"
  [{:keys [payload type length flags]
    :as frame}
   settings]
  (let [padding (inc (get payload 0))
        length (- length padding)
        flags (disj flags :padded)
        payload (take length (rest payload))]
    (-> frame
        (merge
         {:length length
          :flags flags
          :padding padding
          :payload (byte-array payload)})
        (validate-padding settings))))

(defn- validate-header
  "Validate the common frame header"
  [{:keys [type length stream increment] :as frame}
   & [{:keys [settings-max-frame-size]}]]
  {:pre [type length stream]}
  (let [code (spec/frame-code type)
        flags (spec/frame-flag-index type)]
    (when-not code
      (raise :compression-error "Invalid frame type: " type))
    (when (> length settings-max-frame-size)
      (raise :compression-error "Frame size is too large: " length))
    (when (< length 0)
      (raise :compression-error "Frame size is invalid: " length))
    (when (> stream spec/max-stream-id)
      (raise :compression-error "Stream ID is too large: " stream))
    (when (and (= type :window-update) (> increment spec/max-windowinc))
      (raise :compression-error "Window increment is too large: " increment))
    frame))

(def settings-template
  "Frame codec type to encode and decode settings payload in http2 frames"
  (let [kvp-tpl
          (frame-type
            (frame-encoder [value]
                           sk (short-type) (let [sk (-> value first spec/setting-code)]
                                             (when-not sk
                                               (raise :compression-error "Unknown settings type: " sk))
                                             sk)
                           sv (int32-type) (-> value last))
            (frame-decoder [buf offset]
                           sk (short-type)
                           sv (int32-type))
            (fn [[sk sv]]
              [(spec/setting-type sk) sv]))]
    (frame-type
      (frame-encoder [value]
                     settings (repeated-frame kvp-tpl (count value)) value)
      (frame-decoder [buf offset]
                     settings (repeated-frame kvp-tpl (/ (-> buf (. capacity)) 6)))
      (fn [settings]
        settings))))

(def frame-template
  "Frame codec type to encode and decode http2 frames"
  (frame-type
   (frame-encoder [value]
                  length (medium-type) (:length value)
                  type (byte-type) (-> value :type spec/frame-code)
                  flags (byte-type) (reduce (fn [c f]
                                              (let [type (:type value)
                                                    flags (spec/frame-flag-index type)
                                                    position (f flags)]
                                                (when-not position
                                                  (raise :compression-error "Invalid frame flag: " f " for frame type: " type))
                                                (bit-or c (bit-shift-left 1 position))))
                                            0x0 (:flags value))
                  stream (int32-type) (:stream value)
                  payload (bytes-type (:length value)) (:payload value))
   (frame-decoder [buf offset]
                  length (medium-type)
                  type (byte-type)
                  flags (byte-type)
                  stream (int32-type)
                  payload (bytes-type (read length buf offset)))
   (fn [[length type flags stream payload]]
     (let [tval (spec/frame-type type)
           fmap (spec/frame-index-flag tval)
           fbin (reverse (to-bit-map (byte-type) flags))
           fval (->> fbin
                     (map-indexed (fn [i v] (if v (get fmap i))))
                     (filter (comp not nil?))
                     (set))]
       {:length length
        :type tval
        :flags fval
        :stream stream
        :payload payload}))))

(defn- buffer-to-frame
  "Converts a buffer to a frame using the `frame-template"
  [buffer]
  (let [tpl (dynamic-buffer frame-template)
        [frame] (decompose tpl buffer)]
    frame))

(defn- frame-to-buffer
  "Converts a frame to a buffer using the `frame-template"
  [frame]
  (let [tpl (dynamic-buffer frame-template)
        buffer (compose tpl [frame])]
    buffer))

(defmulti encode-frame :type)

(defmulti decode-frame :type)

(defmethod encode-frame :data
  [{:keys [payload padding flags]
    :as frame}
   & [settings]]
  (-> frame
      (assoc :length (count payload))
      (cond-> (and padding (not (contains? flags :padded)))
              (encode-padding settings))))

(defmethod decode-frame :data
  [{:keys [type flags]
    :as frame}
   & [settings]]
  (cond-> frame
          (and (spec/frame-padding? type)
               (contains? flags :padded))
          (decode-padding settings)))

(defmethod encode-frame :headers
  [{:keys [payload padding flags weight stream-dependency exclusive]
    :as frame}
   & [settings]]
  (-> frame
      (assoc :length (count payload))
      (cond-> (or weight stream-dependency (true? exclusive))
              (encode-priority settings)
              (and padding (not (contains? flags :padded)))
              (encode-padding settings))))

(defmethod decode-frame :headers
  [{:keys [type flags]
    :as frame}
   & [settings]]
  (cond-> frame
          (and (spec/frame-padding? type)
               (contains? flags :padded))
          (decode-padding settings)
          (and (spec/frame-priority? type)
               (contains? flags :priority))
          (decode-priority settings)))

(defmethod encode-frame :priority
  [frame
   & [settings]]
  (-> frame
      (assoc :length 0)
      (assoc :payload (byte-array 0))
      (encode-priority settings)))

(defmethod decode-frame :priority
  [frame
   & [settings]]
  (-> frame
      (decode-priority settings)))

(defmethod encode-frame :rst-stream
  [frame
   & [settings]]
  (-> frame
      (assoc :length 0)
      (assoc :payload (byte-array 0))
      (encode-error settings)))

(defmethod decode-frame :rst-stream
  [frame
   & [settings]]
  (-> frame
      (decode-error settings)))

(defmethod encode-frame :settings
  [{:keys [stream settings]
    :as frame}
   & _ ]
  (when (not= stream 0)
    (raise :protocol-error "Invalid Stream ID: " stream))
  (let [tpl (dynamic-buffer settings-template)
        length (-> settings count (* 6))
        setbuf (compose tpl [settings])
        payload (read (bytes-type length) setbuf 0)]
    (-> frame
        (assoc :length length)
        (assoc :payload payload))))

(defmethod decode-frame :settings
  [{:keys [length payload stream]
    :as frame}
   & _ ]
  (when (not= stream 0)
    (raise :protocol-error "Invalid Stream ID: " stream))
  (when (not= (mod length 6) 0)
    (raise :protocol-error "Invalid settings payload length: " length))
  (let [tpl (dynamic-buffer settings-template)
        [[settings]] (decompose tpl (wrapped-buffer payload))]
    (-> frame
        (assoc :length 0)
        (assoc :payload (byte-array 0))
        (assoc :settings (into {} settings)))))

(defmethod encode-frame :push-promise
  [{:keys [payload padding flags promise-stream]
    :as frame}
   & [settings]]
  {:pre [frame]}
  (let [psval (bit-and promise-stream reserved-bit)
        erbuf (-> (spec :promise-stream (int32-type))
                  (compose-buffer)
                  (set-field :promise-stream psval)
                  (buffer))
        pbytes (read (bytes-type 4) erbuf 0)
        payload (concat pbytes payload)
        length (count payload)]
    (-> frame
        (merge
          {:length length
           :payload (byte-array payload)})
        (cond-> (and padding (not (contains? flags :padded)))
                (encode-padding settings)))))

(defmethod decode-frame :push-promise
  [{:keys [type flags payload]
    :as frame}
   & [settings]]
  (let [{:keys [type flags payload length]
         :as frame} (cond-> frame (and (spec/frame-padding? type)
                                       (contains? flags :padded))
                            (decode-padding settings))
        length (- length 4)
        psbuf (-> (spec :promise-stream (int32-type)
                        :payload (bytes-type length))
                  (compose-buffer :orig-buffer payload))
        psval (-> psbuf (get-field :promise-stream) (bit-and reserved-bit))
        payload (get-field psbuf :payload)]
    (merge frame
           {:length length
            :promise-stream psval
            :payload payload})))

(defmethod encode-frame :ping
  [{:keys [payload stream]
    :as frame}
   & [settings]]
  (let [length (count payload)]
    (when (not= stream 0)
      (raise :protocol-error "Invalid Stream ID: " stream))
    (when (not= length 8)
      (raise :protocol-error "Invalid ping payload size: " length))
    (-> frame
        (assoc :length length))))

(defmethod decode-frame :ping
  [{:keys [stream length]
    :as frame}
   & [settings]]
  (when (not= stream 0)
    (raise :protocol-error "Invalid Stream ID: " stream))
  (when (not= length 8)
    (raise :frame-size-error "Invalid ping payload size: " length))
  frame)

(defmethod encode-frame :goaway
  [{:keys [last-stream]
    :as frame}
   & [settings]]
  (let [lsval (bit-and last-stream reserved-bit)
        lsbuf (-> (spec :last-stream (int32-type))
                  (compose-buffer)
                  (set-field :last-stream lsval)
                  (buffer))
        sbytes (read (bytes-type 4) lsbuf 0)
        {:keys [payload] :as frame} (encode-error frame settings)
        payload (concat sbytes payload)]
    (-> frame
        (merge
          {:length (count payload)
           :payload (byte-array payload)}))))

(defmethod decode-frame :goaway
  [{:keys [type flags length payload]
    :as frame}
   & [settings]]
  (let [lsbuf (-> (spec :last-stream (int32-type)
                        :payload (bytes-type (- length 4)))
                  (compose-buffer :orig-buffer payload))
        lsval (-> lsbuf (get-field :last-stream) (bit-and reserved-bit))
        payload (get-field lsbuf :payload)]
    (-> frame
        (merge
          {:last-stream lsval
           :length (count payload)
           :payload payload})
        (decode-error settings))))

(defmethod encode-frame :default
  [{:keys [type]} & _]
  (raise :compression-error "Unsupported frame type: " type))

(defmethod decode-frame :default
  [{:keys [type]} & _]
  (raise :compression-error "Unsupported frame type: " type))

(defn get-buffer
  "Gets a buffer from a frame"
  [{:keys [flags stream] :as frame
    :or {flags #{} stream 0}}
   & [{:keys [settings]
       :or {settings (spec/setting-init)}}]]
  (-> (merge frame {:flags flags :stream stream})
      (encode-frame settings)
      (validate-header settings)
      (frame-to-buffer)))

(defn get-frame
  "Gets a frame from a buffer"
  [buffer
   & [{:keys [settings]
       :or {settings (spec/setting-init)}}]]
  (-> (buffer-to-frame buffer)
      (decode-frame settings)
      (validate-header settings)))

(let [payload (b "This is an implementation of HTTP2")
      frame {:type :goaway
             :stream 1
             :last-stream 10
             :error :protocol-error
             :flags #{}
             :payload payload}]
  (-> frame
      get-buffer
      get-frame))
