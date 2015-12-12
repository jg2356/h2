(ns h2.protocol.error
  (:require [slingshot.slingshot :refer [throw+ try+]]
            [h2.protocol.spec :refer [error-code]]))

(defn raise
  "Throws the error exception with the given type and info."
  [type & info]
  {:pre [(error-code type)]}
  (let [e {:error type
           :message (apply str info)}]
    (throw+ e)))

(defn raised?
  "Determines if an exception was raised with error-id."
  ([type]
  {:pre [(error-code type)]}
   (fn [exception]
     (raised? type exception)))
  ([type exception]
   {:pre [(error-code type)]}
   (isa? (:error exception) type)))
