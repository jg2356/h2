(ns h2.protocol.tcp
  (:require [clojure.java.io :as io]
            [h2.protocol.spec :as spec]
            [h2.protocol.common :refer :all]
            [h2.protocol.frame :refer :all]
            [h2.protocol.error :refer :all]
            [slingshot.slingshot :refer [try+]]
            [clojurewerkz.buffy.util :refer :all]
            [clojurewerkz.buffy.core :as buffy
             :exclude [buffer]
             :refer :all]
            [clojurewerkz.buffy.types.protocols :refer :all]
            [clojurewerkz.buffy.frames :refer :all])
  (:import [java.net InetAddress ServerSocket Socket SocketException]))

(defn server
  "Create a new TCP server:
  :host    - the host to bind to (defaults to 127.0.0.1)
  :port    - the port to bind to
  :backlog - the number of backlog connections
  :handler - the handler for received frames `(fn [connection frame] ...)"
  [& {:keys [host port backlog handler]
      :or {host "127.0.0.1"
           backlog 100}
      :as options}]
  {:pre [host port handler]}
  (merge
    options
    {:host host
     :socket (atom nil)
     :connections (atom #{})
     :backlog backlog}))

(defn client
  "Create a new TCP client:
  :host    - the host to connect to
  :port    - the port to connect to
  :handler - the handler for incoming frames `(fn [frame] ...)"
  [& {:keys [host port handler]
      :as options}]
  {:pre [host port handler]}
  (merge
    options
    {:socket (atom nil)
     :settings (atom (spec/setting-init))}))

(defn transmit
  "Transmits data over an endpoint's TCP socket"
  [{:keys [socket]
    :as endpoint}
   data]
  {:pre [@socket]}
  (let [out-stream (.getOutputStream @socket)]
    (doto out-stream
      (.write data)
      (.flush))))

(defn receive
  "Receives data from an endpoint's TCP socket"
  [{:keys [socket]
    :as endpoint}
   length]
  {:pre [@socket]}
  (let [in-stream (.getInputStream @socket)
        data (byte-array length)]
    (.read in-stream data)
    data))

(defn transmit-preface
  "Transmits the h2 connection preface over an endpoint's TCP socket"
  [endpoint]
  (transmit endpoint spec/h2-preface-bytes))

(defn receive-preface
  "Returns true if the h2-preface was correctly received over an endpoint's TCP socket"
  [endpoint]
  (let [data (receive endpoint spec/h2-preface-length)
        text (s data)]
    (= text spec/h2-preface)))

(defn transmit-frame
  "Transmits an unpacked h2 frame over an endpoint's TCP socket"
  [{:keys [settings] :as endpoint}
   frame]
  (let [fbuf (buffer (pack frame settings))
        flen (.capacity fbuf)
        fbytes (read (bytes-type flen) fbuf 0)]
    (.release fbuf)
    (transmit endpoint fbytes)))

(defn receive-frame
  "Receive an h2 frame over an endpoint's TCP socket if available"
  [{:keys [settings] :as endpoint}]
  (let [hbytes (receive endpoint spec/h2-header-length)
        hbuf (wrapped-buffer hbytes)
        hval (header hbuf)
        pbytes (receive endpoint (first hval))]
    (.release hbuf)
    (unpack (frame hval pbytes) settings)))

(defn running?
  "True if the client/server socket is open."
  [{:keys [socket]}]
  (and @socket (not (.isClosed @socket))))

(defn close-connection
  [{:keys [connections]
    :as server}
   {:keys [socket]
    :as connection}]
  {:pre [@connections @socket]}
  "Close a TCP socket connection."
  (swap! connections disj connection)
  (when (running? connection)
    (.close @socket)))

(defn accept-connection
  "Accept a TCP socket connection."
  [{:keys [connections socket handler] :as server}]
  {:pre [@connections @socket handler]}
  (let [client-socket (.accept @socket)
        connection {:socket (atom client-socket)
                    :settings (atom (spec/setting-init))}]
    (swap! connections conj connection)
    (future
      (try
        (when (receive-preface connection)
          (while true
            (handler connection (receive-frame connection))))
        (finally
          (close-connection server connection))))))

(defn start-server
  "Start a TCP server and start accepting connections."
  [{:keys [socket host port backlog handler]
    :as server}]
  {:pre [(not @socket) host port backlog handler]}
  (->> (InetAddress/getByName host)
       (new ServerSocket port backlog)
       (reset! socket))
  (future
    (while (running? server)
      (try
        (accept-connection server)
        (catch SocketException _)))))

(defn stop-server
  "Stop the TCP server and close all open connections."
  [{:keys [socket connections]
    :as server}]
  {:pre [@socket @connections]}
  (doseq [connection @connections]
    (close-connection server connection))
  (.close @socket))

(defn stop-client
  "Stop a TCP client connection to the target server"
  [{:keys [socket]}]
  {:pre [@socket]}
  (.close @socket))

(defn start-client
  "Start a TCP client connection to the target server"
  [{:keys [socket host port handler]
    :as client}]
  {:pre [(not @socket) host port handler]}
  (->> (new Socket host port)
       (reset! socket))
  (future
    (while (running? client)
      (try
        (transmit-preface client)
        (while true
          (handler (receive-frame client)))
        (finally
          (stop-client client))))))
