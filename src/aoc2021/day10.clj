(ns aoc2021.day10
  (:require [clojure.string :as s])
  (:require [aoc2021.util :as u]))

;;;
;;; part 1
;;;

(def bracket-pairs {\( \), \[ \], \{ \}, \< \>})
(def bracket-score-illegal {\) 3, \] 57, \} 1197, \> 25137})

(defn analyze-line
  "Input: Line of brackets
  Output: Map with current/final :pos, optional :error and optional stack of unclosed opened brackets (lvl->char)"
  [line]
  (reduce (fn[m c] ; m is both our "stack" and where we put :pos and :error
            (if (m :error) m ; if we already have error skip all other chars
              (let [l (dec (count m))] ; l is kinda our stack height (level)
                (cond
                  ; opening brackets:
                  (#{\( \[ \{ \<} c)
                    (assoc m (inc l) c :pos (inc (m :pos)))
                  ; closing bracket that match a previously opened bracket:
                  (= c (bracket-pairs (m l)))
                    (assoc (dissoc m l) :pos (inc (m :pos)))
                  ; non-matching closing bracket:
                  :else
                  (let [err-pos (inc (m :pos))]
                    (assoc m
                           :pos err-pos
                           :error {:actual c
                                   :expected (bracket-pairs (m l))
                                   :msg (str "At " err-pos ": "
                                             "Expected " (bracket-pairs (m l))
                                             ", but was " c)}))))))
          {:pos 0}
          (seq line)))

(defn solve-part-1
  "Syntax Scoring: Find illegal chunks"
  [data]
  (->> data
       (map analyze-line)
       (filter :error) ; only want errors here
       (map #(get-in % [:error :actual])) ; extract the erroneous bracket
       (map bracket-score-illegal) ; get it's score
       (apply +))) ; and sum up all error bracket scores

;;;
;;; part 2
;;;

(def bracket-score-autocomplete {\) 1, \] 2, \} 3, \> 4})

(defn calc-autocomplete-score
  "Input: (c ...) ; list of the four opening brackets
  Output: [\"c...\" n] ; vector of string representation and score"
  [obrackets]
  (let [cbrackets (map bracket-pairs obrackets)]
    [(apply str cbrackets)
     (->> cbrackets
       (map bracket-score-autocomplete)
       (reduce #(+ (* %1 5) %2)))]))

(defn solve-part-2
  "Syntax Scoring: Complete incomplete chunks"
  [data]
  (->> data
       (map analyze-line)
       (filter (complement :error)) ; don't want errors here
       (map #(dissoc % :pos)) ; remove :pos from every line
       (map #(sort-by (comp - first) %)) ; sort by level (descending)
       (map #(map second %)) ; discard level numbers
       (map calc-autocomplete-score) ; calc score for every line
       (sort-by second) ; sort by score
       (#(nth % (quot (count %) 2))))) ; get element in the middle
