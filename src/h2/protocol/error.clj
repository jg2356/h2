(ns h2.protocol.error
  (:require [slingshot.slingshot :refer [throw+ try+]]))

;; critical errors
(derive ::protocol-error ::error)
(derive ::handshake-error ::error)
(derive ::compression-error ::protocol-error)
(derive ::flow-control-error ::protocol-error)
(derive ::internal-error ::protocol-error)

;; non-critical errors
(derive ::stream-closed ::error)
(derive ::connection-closed ::error)
(derive ::stream-limit-exceeded ::error)

(defn raise
  "Throws the error exception with the given id and info."
  [id & info]
  (let [e (apply hash-map :error-id id info)]
    (throw+ e)))

(defn raised?
  "Determines if an exception was raised with error-id."
  ([error-id]
   (fn [exception]
     (raised? exception error-id)))
  ([exception error-id]
   (isa? (:error-id exception) error-id)))
