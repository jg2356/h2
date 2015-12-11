(ns h2.protocol.error
  (:require [slingshot.slingshot :refer [throw+ try+]]))

(defn raise
  "Throws the error exception with the given type and info."
  [type & info]
  (let [e {:error type
           :message (apply str info)}]
    (throw+ e)))

(defn raised?
  "Determines if an exception was raised with error-id."
  ([type]
   (fn [exception]
     (raised? exception type)))
  ([exception type]
   (isa? (:error exception) type)))
