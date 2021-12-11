(ns aoc2021.day03
  (:require [aoc2021.util :as u]))

;;; part 1

(defn count-bits
  "Counts 1 bits of a list of strings.
  Example: (count-bits '(\"001\" \"011\" \"111\")) yields (1 2 3)"
  [bit-strings]
  (reduce (fn[counts bits] (map #(if (= %2 \1) (inc %1) %1) counts bits))
          (repeat 0)
          bit-strings))

(defn counts-to-binary
  "Returns 1 for all numbers in counts that are greater than or equal to
  min-for-1. Numbers must be less than 2 times min-for-1.
  Example: (counts-to-binary '(0 1 2 3 4 5) 3) yields (0 0 0 1 1 1)"
  [counts min-for-1]
  (map #(if (>= % min-for-1) 1 0) counts))

(defn invert-bits
  "Inverts a list of zeros and ones.
  Example: (invert-bits '(0 1 0 1)) yields (1 0 1 0)"
  [bits]
  (map #(if (= % 0) 1 0) bits))

(defn solve-part-1
  "bit strings"
  [bit-strings]
  (let [bit-result (-> bit-strings
                       count-bits
                       (counts-to-binary (/ (count bit-strings) 2)))
        gamma-rate (u/binary-to-number bit-result)
        epsilon-rate (u/binary-to-number (invert-bits bit-result))]
    {:gamma-rate-bin (Integer/toBinaryString gamma-rate)
     :gamma-rate-int gamma-rate
     :epsilon-rate-bin (Integer/toBinaryString epsilon-rate)
     :epsilon-rate-int epsilon-rate
     :power-consumption (* gamma-rate epsilon-rate)}))

;;; part 2

(defn reduce-by-pos
  "Reduce by bit criteria as described in part 2 of day 3.
  Example: (reduce-by-pos '(\"001\" \"011\" \"111\") :more \1) yields 111,
  (reduce-by-pos '(\"101\" \"010\" \"110\" \"011\") :more \1) yields 110"
  ([bit-strings more-or-less char-if-equal]
   (reduce-by-pos bit-strings more-or-less char-if-equal 0))
  ([bit-strings more-or-less char-if-equal pos]
   ; if bit-strings has only one element (rest is empty)
   ;   then return that element
   ;   else reduce again
   (if (empty? (rest bit-strings)) 
     (first bit-strings)
     (let [grouped (group-by #(.charAt % pos) bit-strings)
           zeroes (count (grouped \0))
           ones (count (grouped \1))
           cmp ({:more > :less <} more-or-less)]
       (recur (cond 
                (= zeroes ones) (grouped char-if-equal)
                (cmp zeroes ones) (grouped \0)
                :else (grouped \1))
              more-or-less
              char-if-equal
              (inc pos))))))

(defn solve-part-2
  [bit-strings]
  (let [oxygen-bin (reduce-by-pos bit-strings :more \1)
        co2-bin (reduce-by-pos bit-strings :less \0)
        oxygen-int (Integer/parseInt oxygen-bin 2)
        co2-int (Integer/parseInt co2-bin 2)]
    {:oxygen-bin oxygen-bin
     :oxygen-int oxygen-int
     :co2-bin co2-bin
     :co2-int co2-int
     :life-support (* oxygen-int co2-int)}))
