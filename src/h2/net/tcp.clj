(ns h2.net.tcp
  (:require [clojure.java.io :as io])
  (:import [java.net InetAddress ServerSocket Socket SocketException]))

(defn server
  "Create a new TCP server:
  :host    - the host to bind to (defaults to 127.0.0.1)
  :port    - the port to bind to"
  [& {:as options}]
  {:pre [(:port options)]}
  (merge
    {:host "127.0.0.1"
     :backlog 100
     :socket (atom nil)
     :connections (atom #{})}
    options))

(defn server-socket [server]
  "Create a TCP server socket."
  (ServerSocket.
    (:port server)
    (:backlog server)
    (InetAddress/getByName (:host server))))

(defn accept-connection
  "Accept a TCP socket connection."
  [{:keys [connections socket] :as server}]
  (let [conn (.accept @socket)]
    (swap! connections conj conn)))

(defn close-connection [server socket]
  "Close a TCP socket connection."
  (swap! (:connections server) disj socket)
  (when-not (.isClosed socket)
    (.close socket)))

(defn running?
  "True if the server is running."
  [server]
  (if-let [socket @(:socket server)]
    (not (.isClosed socket))))

(defn start-server
  "Start a TCP server and start accepting connections."
  [server]
  (reset! (:socket server) (server-socket server))
  (future
    (while (running? server)
      (try
        (accept-connection server)
        (catch SocketException _)))))

(defn stop-server
  "Stop the TCP server and close all open connections."
  [server]
  (doseq [socket @(:connections server)]
    (close-connection server socket))
  (.close @(:socket server)))
