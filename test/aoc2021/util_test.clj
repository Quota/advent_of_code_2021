(ns aoc2021.util-test
  (:require [clojure.test :refer :all]
            [aoc2021.util :refer :all]))

(deftest binary-to-number-empty-test
  (testing "binary-to-number"
    (is (= 0 (binary-to-number '())))))

(deftest binary-to-number-0-test
  (testing "binary-to-number"
    (is (= 0 (binary-to-number '(0))))))

(deftest binary-to-number-1-test
  (testing "binary-to-number"
    (is (= 1 (binary-to-number '(1))))))

(deftest binary-to-number-2-test
  (testing "binary-to-number"
    (is (= 2 (binary-to-number '(1 0))))))

(deftest binary-to-number-3-test
  (testing "binary-to-number"
    (is (= 3 (binary-to-number '(1 1))))))

(deftest binary-to-number-4-test
  (testing "binary-to-number"
    (is (= 4 (binary-to-number '(1 0 0))))))

(deftest binary-to-number-255-test
  (testing "binary-to-number"
    (is (= 255 (binary-to-number '(1 1 1 1 1 1 1 1))))))
