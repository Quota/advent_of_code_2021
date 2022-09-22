(ns aoc2021.day14
  (:require [clojure.string :as s])
  (:require [aoc2021.util :as u]))

;;;
;;; misc
;;;

(defn read-data
  "Reads the polymer template and insertion rules.
  Note that instead of returning the polymer template as a string this 
  functions returns the _quantities_ of element pairs (for further processing)
  as well as the insertion rules with complete values (not just the element
  to insert).
  Input: (\"ABC...\" \"\" \"AB -> X\" \"ME -> Y\" ...)
  Output: {:poly-template {\"AB\" 1, \"BC\" 1, ...} 
           :ins-rules {\"AB\" (\"AX\", \"XB\"), \"ME\" (\"MY\", \"YE\"), ...}}"
  [data]
  (let [pt-str (first data)
        ir-str-seq (nnext data)]
    {:poly-template (->> pt-str
                         (partition 2 1)
                         (map (partial apply str))
                         frequencies)
     :last-letter (last pt-str)
     :ins-rules (->> ir-str-seq ; "NC -> B"  =>  [NC, (NB BC)]
                     (map (fn[line] (let [in (.substring line 0 2)
                                         out (.substring line 6)]
                                      [in (list (str (first in) out) (str out (second in)))])))
                     (into {}))}))

(comment
  (read-data
    (clojure.string/split-lines (slurp "resources/day14_input_sample.txt")))
  )

(defn add2
  "Adds the two given values, nil acts as 0."
  [x y]
  (+ (or x 0) (or y 0)))

(defn apply-insertions
  [data]
  (assoc
    data
    :poly-template
    (reduce 
      (fn apply-insertions-reducer [m [pair count]] 
        (if-let [subst-pair (get (:ins-rules data) pair)]
          (-> m (update (first subst-pair) add2 count)
                (update (second subst-pair) add2 count))
          (update m pair add2 count)))
      {}
      (data :poly-template))))

(defn solve-with-iterations
  [iterations data]
  (as-> data $
    ; iterate the insertions...
    (iterate apply-insertions $)
    ; ...as often as requested
    (nth $ iterations)
    ; we only need the polymer template
    (:poly-template $)
    ; sum up all element counts
    (reduce (fn[sums [pair count]] (update sums (first pair) add2 count)) {} $)
    ; (and adjust for the original input's last element)
    (update $ (data :last-letter) add2 1)
    ; again we only need the numbers (not the elements themselves)
    (vals $)
    ; and calculate the diff between max and min quantities
    (- (apply max $) (apply min $))))

;;;
;;; part 1
;;;

(defn solve-part-1
  "day 14 part 1"
  [input]
  (solve-with-iterations 10 (read-data input)))

;;;
;;; part 2
;;;

(defn solve-part-2
  "day 14 part 2"
  [input]
  (solve-with-iterations 40 (read-data input)))

;;;
;;; dev space
;;;

(comment
  (solve-part-1
    (clojure.string/split-lines (slurp "resources/day14_input_real.txt")))

  (solve-part-2
    (clojure.string/split-lines (slurp "resources/day14_input_real.txt")))

  (read-data
    (clojure.string/split-lines (slurp "resources/day14_input_real.txt")))

)
