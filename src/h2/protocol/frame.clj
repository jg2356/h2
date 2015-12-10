(ns h2.protocol.frame
  (:require [clojure.set]
            [h2.protocol.common :refer :all]
            [h2.protocol.error :refer [raise compression-error]]
            [h2.protocol.spec :as spec]
            [clojurewerkz.buffy.util :refer :all]
            [clojurewerkz.buffy.core :refer :all]
            [clojurewerkz.buffy.types.protocols :refer :all]
            [clojurewerkz.buffy.frames :refer :all]))

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

(defn validate-header
  "Validate the common frame header"
  [{:keys [type length stream increment] :as frame}
   & [{:keys [settings-max-frame-size] :as settings
       :or {settings-max-frame-size (spec/setting-init :settings-max-frame-size)}}]]
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

(defmulti encode-frame :type)

(defmulti decode-frame :type)

(defmethod encode-frame :data
  [{:keys [payload]
    :as data}]
  (assoc data :length (count payload)))

(defmethod decode-frame :data
  [frame & _]
  frame)

(defmethod encode-frame :default
  [{:keys [type]} & _]
  (raise compression-error "Unsupported frame type: " type))

(defmethod decode-frame :default
  [{:keys [type]} & _]
  (raise compression-error "Unsupported frame type: " type))

(defn validate-padding
  "Validates frame padding"
  [type length padding settings-max-frame-size]
  (when-not (spec/frame-padding? type)
    (raise compression-error "Invalid padding flag for: " type))
  (when (or (<= padding 0) (> padding 256) (> (+ padding length) settings-max-frame-size))
    (raise compression-error "Invalid padding: " padding)))

(defn encode-padding
  "Encode padding when applicable"
  [{:keys [payload type length padding flags]
    :as frame}
   & [{:keys [settings-max-frame-size] :as settings
       :or {settings-max-frame-size (spec/setting-init :settings-max-frame-size)}}]]
  (validate-padding type length padding settings-max-frame-size)
  (let [length (+ length padding)
        padlen (byte (dec padding))
        flags (conj flags :padded)
        payload (concat [padlen] payload (byte-array padlen))]
    (merge
      frame
      {:length length
       :flags flags
       :payload (byte-array payload)})))

(defn decode-padding
  "Decode padding when applicable"
  [{:keys [payload type length flags]
    :as frame}
   & [{:keys [settings-max-frame-size] :as settings
       :or {settings-max-frame-size (spec/setting-init :settings-max-frame-size)}}]]
  (let [padding (inc (get payload 0))
        length (- length padding)
        flags (disj flags :padded)
        payload (take length (rest payload))]
    (validate-padding type length padding settings-max-frame-size)
    (merge
      frame
      {:length length
       :flags flags
       :padding padding
       :payload (byte-array payload)})))

(defn get-buffer
  [{:keys [type flags stream padding] :as frame
    :or {flags #{} stream 0}}
   & [{:keys [settings] :as connection
       :or {settings spec/setting-defaults}}]]
  (let [tpl (dynamic-buffer frame-template)
        frm (-> (merge frame {:flags flags :stream stream})
                (encode-frame)
                (cond-> (and padding
                             (not (contains? flags :padded)))
                        (encode-padding settings)))]
    (validate-header frm settings)
    (compose tpl [frm])))

(defn get-frame
  [buffer
   & [{:keys [settings] :as connection
       :or {settings spec/setting-defaults}}]]
  (let [tpl (dynamic-buffer frame-template)
        [{:keys [type flags] :as frame}] (decompose tpl buffer)
        frm (-> (decode-frame frame)
                (cond-> (and (spec/frame-padding? type)
                             (contains? flags :padded))
                        (decode-padding settings)))]
    (validate-header frm)))


(let [payload (b "This is an implementation of HTTP2")
      buffer (get-buffer {:type :data
                          :stream 2
                          :padding 66
                          :payload payload})]
  (-> (get-frame buffer)
      (get-buffer)
      (get-frame)
      :payload
      s))
