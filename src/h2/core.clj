(ns h2.core
  (:require [h2.net.tcp :as tcp])
  (:gen-class))

(defn -main
  [& args]
  (let [s (tcp/server :port 1337)
        h (tcp/start-server s)]
    @h))
