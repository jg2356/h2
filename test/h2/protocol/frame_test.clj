(ns h2.protocol.frame-test
  (:require [clojure.test :refer :all]
            [h2.protocol.frame :refer :all]
            [h2.protocol.common :refer :all]))

(def get-buffer
  (comp buffer pack))

(def get-frame
  (comp unpack frame))

(deftest data-frame-test
  (testing "encode and decode data frame"
    (let [text "This is a test to encode and decode a data frame"
          buffer (get-buffer {:type :data
                              :stream 2
                              :padding 66
                              :flags #{:compressed :end-stream}
                              :payload (b text)})
          frame (get-frame buffer)]
      (are [x y] (= x y)
           (-> frame :type)       :data
           (-> frame :stream)     2
           (-> frame :padding)    66
           (-> frame :flags)      #{:compressed :end-stream}
           (-> frame :length)     (count text)
           (-> frame :payload s)  text))))

(deftest headers-frame-test
  (testing "encode and decode headers frame"
    (let [text "This is a test to encode and decode a headers frame"
          buffer (get-buffer {:type :headers
                              :stream 1
                              :padding 50
                              :weight 10
                              :flags #{:end-stream :end-headers}
                              :stream-dependency 1337
                              :exclusive true
                              :payload (b text)})
          frame (get-frame buffer)]
      (are [x y] (= x y)
           (-> frame :type)               :headers
           (-> frame :stream)             1
           (-> frame :padding)            50
           (-> frame :length)             (count text)
           (-> frame :weight)             10
           (-> frame :stream-dependency)  1337
           (-> frame :exclusive)          true
           (-> frame :flags)              #{:priority :end-stream :end-headers}
           (-> frame :payload s)  text))))

(deftest priority-frame-test
  (testing "encode and decode priority frame"
    (let [buffer (get-buffer {:type :priority
                              :stream 5
                              :weight 10
                              :stream-dependency 1337
                              :exclusive true})
          frame (get-frame buffer)]
      (are [x y] (= x y)
           (-> frame :type)               :priority
           (-> frame :stream)             5
           (-> frame :length)             0
           (-> frame :payload s)          ""
           (-> frame :weight)             10
           (-> frame :stream-dependency)  1337
           (-> frame :exclusive)          true
           (-> frame :flags)              #{}))))

(deftest rst-stream-frame-test
  (testing "encode and decode rst-stream frame"
    (let [buffer (get-buffer {:type :rst-stream
                              :stream 6
                              :error :compression-error})
          frame (get-frame buffer)]
      (are [x y] (= x y)
           (-> frame :type)               :rst-stream
           (-> frame :stream)             6
           (-> frame :error)              :compression-error
           (-> frame :length)             0
           (-> frame :payload s)          ""
           (-> frame :flags)              #{}))))

(deftest settings-frame-test
  (testing "encode and decode settings frame"
    (let [buffer (get-buffer {:type :settings
                              :stream 0
                              :settings {:settings-header-table-size       2048
                                         :settings-enable-push             1
                                         :settings-max-concurrent-streams  12345
                                         :settings-initial-window-size     23456
                                         :settings-max-frame-size          16384
                                         :settings-max-header-list-size    45678}})
          frame (get-frame buffer)]
      (are [x y] (= x y)
           (-> frame :type)               :settings
           (-> frame :settings)           {:settings-header-table-size       2048
                                           :settings-enable-push             1
                                           :settings-max-concurrent-streams  12345
                                           :settings-initial-window-size     23456
                                           :settings-max-frame-size          16384
                                           :settings-max-header-list-size    45678}
           (-> frame :stream)             0
           (-> frame :length)             0
           (-> frame :payload s)          ""
           (-> frame :flags)              #{}))))

(deftest push-promise-frame-test
  (testing "encode and decode push-promise frame"
    (let [text "This is a test to encode and decode a push-promise frame"
          buffer (get-buffer {:type :push-promise
                              :stream 9
                              :flags #{:end-headers}
                              :promise-stream 10
                              :padding 71
                              :payload (b text)})
          frame (get-frame buffer)]
      (are [x y] (= x y)
           (-> frame :type)               :push-promise
           (-> frame :stream)             9
           (-> frame :padding)            71
           (-> frame :length)             (count text)
           (-> frame :promise-stream)     10
           (-> frame :flags)              #{:end-headers}
           (-> frame :payload s)  text))))

(deftest ping-frame-test
  (testing "encode and decode ping frame"
    (let [text "ping1234"
          buffer (get-buffer {:type :ping
                              :stream 0
                              :flags #{:ack}
                              :payload (b text)})
          frame (get-frame buffer)]
      (are [x y] (= x y)
           (-> frame :type)               :ping
           (-> frame :stream)             0
           (-> frame :length)             (count text)
           (-> frame :flags)              #{:ack}
           (-> frame :payload s)  text))))

(deftest goaway-frame-test
  (testing "encode and decode goaway frame"
    (let [text "This is a test to encode and decode a goaway frame"
          buffer (get-buffer {:type :goaway
                              :stream 1
                              :last-stream 10
                              :error :protocol-error
                              :payload (b text)})
          frame (get-frame buffer)]
      (are [x y] (= x y)
           (-> frame :type)               :goaway
           (-> frame :error)              :protocol-error
           (-> frame :stream)             1
           (-> frame :last-stream)        10
           (-> frame :length)             (count text)
           (-> frame :flags)              #{}
           (-> frame :payload s)  text))))

(deftest window-update-frame-test
  (testing "encode and decode window-update frame"
    (let [buffer (get-buffer {:type :window-update
                              :stream 3
                              :increment 1337})
          frame (get-frame buffer)]
      (are [x y] (= x y)
           (-> frame :type)               :window-update
           (-> frame :stream)             3
           (-> frame :increment)          1337
           (-> frame :length)             0
           (-> frame :flags)              #{}
           (-> frame :payload s)          ""))))

(deftest continuation-frame-test
  (testing "encode and decode continuation frame"
    (let [text "This is a test to encode and decode a continuation frame"
          buffer (get-buffer {:type :continuation
                              :stream 4
                              :payload (b text)
                              :flags #{:end-headers}})
          frame (get-frame buffer)]
      (are [x y] (= x y)
           (-> frame :type)               :continuation
           (-> frame :stream)             4
           (-> frame :length)             (count text)
           (-> frame :flags)              #{:end-headers}
           (-> frame :payload s)          text))))
