(ns aoc2021.day03-test
  (:require [clojure.test :refer :all]
            [aoc2021.day03 :refer :all]))

;;;
;;; part 1
;;;

;;;
;;; count-bits
;;;
(deftest count-bits-one-0-test
  (testing "count-bits"
    (is (= '(0) (count-bits '("0"))))))

(deftest count-bits-one-1-test
  (testing "count-bits"
    (is (= '(1) (count-bits '("1"))))))

(deftest count-bits-one-1-2-3-test
  (testing "count-bits"
    (is (= '(1 2 3) (count-bits '("001" "011" "111"))))))

(deftest count-bits-one-3-2-1-test
  (testing "count-bits"
    (is (= '(3 2 1) (count-bits '("111" "110" "100"))))))

;;;
;;; counts-to-binary
;;;
(deftest counts-to-binary-0-test
  (testing "3 < 4 gives 0"
    (is (= '(0) (counts-to-binary '(3) 4)))))

(deftest counts-to-binary-1-test
  (testing "5 > 4 gives 1"
    (is (= '(1) (counts-to-binary '(5) 4)))))

(deftest counts-to-binary-0-1-0-1-test
  (testing "4 digits give 0 1 0 1"
    (is (= '(0 1 0 1) (counts-to-binary '(2 5 3 6) 4)))))

(deftest counts-to-binary-0-1-uneven-test
  (testing "using an uneven min-for-1"
    (is (= '(0 1) (counts-to-binary '(4 6) 5)))))

(deftest counts-to-binary-1-same-test
  (testing "edge case, input equal to min-for-1, result 1"
    (is (= '(1) (counts-to-binary '(5) 5)))))

;;;
;;; invert-bits
;;;
(deftest invert-bits-0-test
  (testing "(0) gives (1)"
    (is (= '(1) (invert-bits '(0))))))

(deftest invert-bits-1-test
  (testing "(1) gives (0)"
    (is (= '(0) (invert-bits '(1))))))

(deftest invert-bits-0-1-0-1-test
  (testing "(0 1 0 1) gives (1 0 1 0)"
    (is (= '(1 0 1 0) (invert-bits '(0 1 0 1))))))

;;;
;;; part 2
;;;

;;;
;;; reduce-by-pos
;;;

;; more and \1 stuff
(deftest reduce-by-pos-more-0
  (testing "0 and :more yield 0"
    (is (= "0" (reduce-by-pos '("0") :more \1)))))

(deftest reduce-by-pos-more-1
  (testing "1 and :more yield 1"
    (is (= "1" (reduce-by-pos '("1") :more \1)))))

(deftest reduce-by-pos-more-0-1
  (testing "0 1 and :more yield 1"
    (is (= "1" (reduce-by-pos '("0" "1") :more \1)))))

(deftest reduce-by-pos-more-00-01-10-11
  (testing "00 01 10 11 and :more yield nil"
    (is (= "11" (reduce-by-pos '("00" "01" "10" "11") :more \1)))))

;; less and \0 stuff
(deftest reduce-by-pos-less-0
  (testing "0 and :less yield 0"
    (is (= "0" (reduce-by-pos '("0") :less \0)))))

(deftest reduce-by-pos-less-1
  (testing "1 and :less yield 1"
    (is (= "1" (reduce-by-pos '("1") :less \0)))))

(deftest reduce-by-pos-less-0-1
  (testing "0 1 and :less yield 1"
    (is (= "0" (reduce-by-pos '("0" "1") :less \0)))))

(deftest reduce-by-pos-less-00-01-10-11
  (testing "00 01 10 11 and :less yield nil"
    (is (= "00" (reduce-by-pos '("00" "01" "10" "11") :less \0)))))
