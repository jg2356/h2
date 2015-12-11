(ns h2.protocol.frame
  (:require [clojure.set]
            [h2.protocol.common :refer :all]
            [h2.protocol.error :refer [raise compression-error]]
            [h2.protocol.spec :as spec]
            [clojurewerkz.buffy.util :refer :all]
            [clojurewerkz.buffy.core :refer :all]
            [clojurewerkz.buffy.types.protocols :refer :all]
            [clojurewerkz.buffy.frames :refer :all]))

(def ebit 0x80000000)

(def rbit 0x7fffffff)

(defn- validate-priority
  "Validates frame priority"
  [{:keys [type weight stream-dependency exclusive]
    :as frame}
   & _ ]
  (when-not (and weight stream-dependency (-> exclusive nil? not))
    (raise compression-error "Must specify all of priority parameters for: "type))
  frame)

(defn- encode-priority
  "Encode frame priority"
  [{:keys [type length payload flags weight stream-dependency exclusive]
    :as frame}
   & _]
  {:pre [frame]}
  (let [frame (validate-priority frame)
        flags (cond-> flags (not= type :priority) (conj :priority))
        exval (if exclusive ebit 0)
        sdval (bit-and stream-dependency rbit)
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
        length (+ length 5)]
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
        sdval (bit-and prval rbit)
        exval (bit-and prval ebit)
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
    (raise compression-error "Unknown error type: " error))
  frame)

(defn- encode-error
  "Encode frame error"
  [{:keys [error]
    :as frame}
   & _]
  {:pre [frame]}
  (let [erval (spec/error-code error)
        erbuf (-> (spec :error (int32-type))
                  (compose-buffer)
                  (set-field :error erval)
                  (buffer))
        ebytes (read (bytes-type 4) erbuf 0)
        length 4]
    (-> frame
        (validate-error)
        (merge
          {:length length
           :payload ebytes}))))

(defn- decode-error
  "Decode frame error"
  [{:keys [payload]
    :as frame}
   & _]
  {:pre [frame]}
  (let [erbuf (-> (spec :error (int32-type))
                  (compose-buffer :orig-buffer payload))
        erval (get-field erbuf :error)
        error (or (spec/error-type erval) :internal-error)]
    (-> frame
        (merge
          {:error error
           :length 0
           :payload (byte-array 0)}))))

(defn- validate-padding
  "Validates frame padding"
  [{:keys [type length padding]
    :as frame}
   {:keys [settings-max-frame-size]}]
  {:pre [type length padding settings-max-frame-size]}
  (when-not (spec/frame-padding? type)
    (raise compression-error "Invalid padding flag for: " type))
  (when (or (<= padding 0) (> padding 256) (> (+ padding length) settings-max-frame-size))
    (raise compression-error "Invalid padding: " padding))
  frame)

(defn- encode-padding
  "Encode frame padding"
  [{:keys [payload type length padding flags]
    :as frame}
   settings]
  {:pre [frame settings]}
  (let [length (+ length padding)
        padlen (byte (dec padding))
        flags (conj flags :padded)
        payload (concat [padlen] payload (byte-array padlen))]
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
      (raise compression-error "Invalid frame type: " type))
    (when (> length settings-max-frame-size)
      (raise compression-error "Frame size is too large: " length))
    (when (< length 0)
      (raise compression-error "Frame size is invalid: " length))
    (when (> stream spec/max-stream-id)
      (raise compression-error "Stream ID is too large: " stream))
    (when (and (= type :window-update) (> increment spec/max-windowinc))
      (raise compression-error "Window increment is too large: " increment))
    frame))

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
                                                  (raise compression-error "Invalid frame flag: " f " for frame type: " type))
                                                (bit-or c (bit-shift-left 1 position))))
                                            0x0 (:flags value))
                  stream (int32-type) (:stream value)
                  payload (bytes-type (:length value)) (:payload value))
   (frame-decoder [buffer offset]
                  length (medium-type)
                  type (byte-type)
                  flags (byte-type)
                  stream (int32-type)
                  payload (bytes-type (read length buffer offset)))
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

(defmethod encode-frame :default
  [{:keys [type]} & _]
  (raise compression-error "Unsupported frame type: " type))

(defmethod decode-frame :default
  [{:keys [type]} & _]
  (raise compression-error "Unsupported frame type: " type))

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
      frame {:type :rst-stream
             :stream 2
             :error :protocol-error}
      buffer (get-buffer frame)]
  (-> buffer
      get-frame))
