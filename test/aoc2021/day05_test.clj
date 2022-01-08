(ns aoc2021.day05-test
  (:require [clojure.test :refer :all]
            [aoc2021.day05 :refer :all]))

;;;
;;; part 1
;;;

;;; inc-map-at-point

(deftest inc-map-at-point-empty-1
  (testing "inc-map-at-point 1 1"
    (is (= {[1 1] 1}
           (inc-map-at-point {} [1 1])))))

(deftest inc-map-at-point-existing-inc
  (testing "inc-map-at-point 1 1"
    (is (= {[1 1] 5}
           (inc-map-at-point {[1 1] 4} [1 1])))))

(deftest inc-map-at-point-other-1
  (testing "inc-map-at-point 1 1"
    (is (= {[1 1] 1 [1 2] 3}
           (inc-map-at-point {[1 2] 3} [1 1])))))

;;; auto-range

(deftest auto-range-1
  (testing "auto-range horizontal 1"
    (is (= (list 1)
           (auto-range 1 1)))))

(deftest auto-range-3
  (testing "auto-range horizontal 3"
    (is (= (list 1 2 3)
           (auto-range 1 3)))))

(deftest auto-range-3-inverted
  (testing "auto-range horizontal 3 inverted"
    (is (= (list 3 2 1)
           (auto-range 3 1)))))

;;; fill-map

(deftest fill-map-empty
  (testing "fill-map empty input"
    (is (= {}
           (fill-map false (list))))))

(deftest fill-map-single-point
  (testing "fill-map single point"
    (is (= {[1 1] 1}
           (fill-map false (list (list 1 1 1 1)))))))

(deftest fill-map-horizontal
  (testing "fill-map horizontal"
    (is (= {[1 1] 1 [2 1] 1 [3 1] 1}
           (fill-map false (list (list 1 1 3 1)))))))

(deftest fill-map-horizontal-inverted
  (testing "fill-map horizontal inverted "
    (is (= {[1 1] 1 [2 1] 1 [3 1] 1}
           (fill-map false (list (list 3 1 1 1)))))))

(deftest fill-map-vertical
  (testing "fill-map vertical"
    (is (= {[1 1] 1 [1 2] 1 [1 3] 1}
           (fill-map false (list (list 1 1 1 3)))))))

(deftest fill-map-vertical-inverted
  (testing "fill-map vertical inverted"
    (is (= {[1 1] 1 [1 2] 1 [1 3] 1}
           (fill-map false (list (list 1 3 1 1)))))))

(deftest fill-map-two-different-points
  (testing "fill-map two different points"
    (is (= {[1 1] 1 [3 4] 1}
           (fill-map false (list (list 1 1 1 1) (list 3 4 3 4)))))))

(deftest fill-map-two-same-points
  (testing "fill-map two same points"
    (is (= {[3 4] 2}
           (fill-map false (list (list 3 4 3 4) (list 3 4 3 4)))))))

; layout:
; 0-1-2-3-4-5 (x)
; |
; 1
; |
; 2     s <--first vector
; |     |
; 3   s-+-e <--second vector
; |     |
; 4     e
; |
; 5
; (y)
(deftest fill-map-two-intersecting-lines
  (testing "fill-map with two intersecting lines"
    (is (= {[3 2] 1
            [3 3] 2
            [3 4] 1
            [2 3] 1
            [4 3] 1}
           (fill-map false (list (list 3 2 3 4) (list 2 3 4 3)))))))

; layout:
; 0-1-2-3-4-5 (x)
; |
; 1
; |
; 2 s-s===e-e
; | ^ ^--second vector
; 3 |
; | +-first vector
; 4
; (y)
(deftest fill-map-two-overlapping-lines
  (testing "fill-map with two overlapping lines"
    (is (= {[1 2] 1
            [2 2] 2
            [3 2] 2
            [4 2] 2
            [5 2] 1}
           (fill-map false (list (list 1 2 5 2) (list 2 2 4 2)))))))

; layout:
; 0-1-2-3-4-5 (x)
; |
; 1   s
; |    \
; 2     \
; |      \
; 3       e
; (y)
(deftest fill-map-single-diagonal
  (testing "fill-map with diagonal line (not supported yet)"
    (is (= {}
           (fill-map false (list (list 2 1 4 3)))))))
