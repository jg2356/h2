(ns h2.protocol.event)

(defn register [listeners event handler & opts]
  {:pre [(instance? clojure.lang.Atom store) event handler]}
  (let [{:keys [once]} (apply hash-map opts)
        listener {:is-once (or once false)
                  :handler handler}]
    (swap! listeners update-in [event] conj listener)))

(defn register-once [listeners event handler]
  (register listeners event handler :once true))

(defn dispatch [listeners event & args]
  {:pre [(instance? clojure.lang.Atom store) event]}
  (let [invoke (fn [handler]
                 (apply handler args))
        dispatch (fn [listeners]
                   (doseq [handler (map :handler listeners)]
                     (invoke handler))
                   (filter (comp not :is-once) listeners))]
    (swap! listeners update-in [event] dispatch)))
