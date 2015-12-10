(ns h2.protocol.error
  (:require [slingshot.slingshot :refer [throw+ try+]]))

;; define error values
(def error ::error)
(def protocol-error ::protocol-error)
(def handshake-error ::handshake-error)
(def compression-error ::compression-error)
(def flow-control-error ::flow-control-error)
(def internal-error ::internal-error)
(def stream-closed ::stream-closed)
(def connection-closed ::connection-closed)
(def stream-limit-exceeded ::stream-limit-exceeded)

(derive protocol-error error)
(derive handshake-error error)
(derive compression-error protocol-error)
(derive flow-control-error protocol-error)
(derive internal-error protocol-error)
(derive stream-closed error)
(derive connection-closed error)
(derive stream-limit-exceeded error)

(defn raise
  "Throws the error exception with the given id and info."
  [id & info]
  (let [e {:error-id id
           :message (apply str info)}]
    (throw+ e)))

(defn raised?
  "Determines if an exception was raised with error-id."
  ([error-id]
   (fn [exception]
     (raised? exception error-id)))
  ([exception error-id]
   (isa? (:error-id exception) error-id)))
