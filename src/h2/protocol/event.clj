(ns h2.protocol.event)

(defn register
  "Registers an event handler"
  [listeners event handler & opts]
  {:pre [(instance? clojure.lang.Atom listeners) event handler]}
  (let [listener (merge (apply hash-map opts)
                        {:handler handler})]
    (swap! listeners update-in [event] conj listener)))

(defn dispatch
  "Dispatches an event"
  [listeners event & args]
  {:pre [(instance? clojure.lang.Atom listeners) event]}
  (let [invoke (fn [handler]
                 (apply handler args))
        dispatch (fn [listeners]
                   (doseq [handler (map :handler listeners)]
                     (invoke handler))
                   (filter (comp not :once) listeners))]
    (swap! listeners update-in [event] dispatch)))
