(ns h2.protocol.frame-test
  (:require [clojure.test :refer :all]
            [h2.protocol.frame :refer :all]
            [h2.protocol.common :refer :all]))

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
           (-> frame :weight)             10
           (-> frame :stream-dependency)  1337
           (-> frame :exclusive)          true
           (-> frame :flags)              #{}))))
