(ns aoc2021.util-test
  (:require [clojure.test :refer :all]
            [aoc2021.util :refer :all]))

;;;
;;; binary-to-number
;;;
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

;;;
;;; parse-numbers
;;;
(deftest parse-numbers-empty
  (testing "parse-numberw with empty input"
    (is (= '() (parse-numbers '())))))

(deftest parse-numbers-single-number
  (testing "parse-numberw with one single number"
    (is (= (list (list 1)) (parse-numbers '("1"))))))

(deftest parse-numbers-single-line
  (testing "parse-numberw with one single line"
    (is (= (list (list 1 2 3)) (parse-numbers '("1 2 3"))))))

(deftest parse-numbers-multi-lines
  (testing "parse-numberw with multiple lines"
    (is (= (list (list 1 2 3)(list 4 5 6)(list 7 8 9))
           (parse-numbers '("1 2 3" "4 5 6" "7 8 9"))))))

(deftest parse-numbers-multi-seps
  (testing "parse-numberw with multiple separators"
    (is (= (list (list 1 2 3)(list 4 5 6)(list 7 8 9))
           (parse-numbers #"[-> :]+" '("1: 2 -> 3" "4: 5 -> 6" "7: 8 -> 9"))))))
