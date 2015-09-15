(ns stblang.core-test
  (:require [clojure.test :refer :all]
            [stblang.core :refer :all]))

(deftest match-test-number1
  (testing "match number"
    (is (:type (match :number "3")) :number)))

(deftest match-test-number2
  (testing "match number"
    (is (:type (match :number "  3")) :number)))


(deftest match-test-notnumber2
  (testing "match number"
    (nil? (match :number "  ss"))))

