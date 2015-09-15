(ns stblang.core-test
  (:require [clojure.test :refer :all]
            [stblang.core :refer :all]))

(def not-nil? (complement nil?))

(deftest match-test-number1
  (testing "match number"
    (is (:type (match :number "3")) :number)))

(deftest match-test-number2
  (testing "match number"
    (is (:type (match :number "  3")) :number)))


(deftest match-test-notnumber2
  (testing "match number"
    (assert (nil? (match :number "  ss")))))

(deftest match-test-multiple
  (testing "match number"
    (assert (not-nil? (match [:number] "  11")))))

(deftest match-test-multiple2
  (testing "match number"
    (assert (nil? (match [:number] "  1 d")))
    (assert (nil? (match [:number :number] "  1 d")))
    (assert (not-nil? (match [:number :id] "  1 d")))))

(deftest match-test-assign
  (testing "match number"
   (assert (not-nil? (match [:id :equals :number] "  d=1")))
    (assert (not-nil? (match :assignment "d = 1")))))



