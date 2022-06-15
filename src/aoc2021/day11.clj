(ns aoc2021.day11
  (:require [clojure.string :as s])
  (:require [aoc2021.util :as u]))

;;;
;;; misc
;;;

;;; functions regarding our data

(defn read-data 
 "Reads the data into an internal representation (a byte array)."
 [data-as-lines]
 (let [data-as-string (clojure.string/join "" data-as-lines)
       arr (byte-array (apply + (map count data-as-lines)))]
  (doseq [i (range 100)]
   ;; put data into `arr`
   (aset-byte arr i (Byte/parseByte (.substring data-as-string i (inc i)))))
  arr))

(defn get-data
  "Get a value from the data. Reading outside of the data returns 
  the default value (nil if omitted)."
  ([data row col] (get-data data row col nil))
  ([data row col defval]
   (if (and (>= row 0) (< row 10) (>= col 0) (< col 10))
     (aget data (+ (* 10 row) col))
     defval)))

(defn set-data!
 "Set a value in the data, and return that value. Writing outside of the
 data has no effect and will return nil."
 [data row col v]
 (if (and (>= row 0) (< row 10) (>= col 0) (< col 10))
  (aset-byte data (+ (* 10 row) col) v)
  nil))

(defn swap-data!
 [data row col f]
 (if (and (>= row 0) (< row 10) (>= col 0) (< col 10))
  (aset-byte data (+ (* 10 row) col) (f (get-data data row col)))
  nil))

(defn swap-all-data!
  [data f]
  (doseq [i (range 100)]
   (aset-byte data i (f (aget data i))))
  data)
  
(defn do-data
  "Execute a function (for side-effects) for every element of the data"
  [data f]
  (doseq [x data]
   (f x)))

(defn format-data
  "Format the data from the internal representation (byte array) to
  a readable string."
  ([barr] (format-data barr " "))
  ([barr sep]
   (clojure.string/join sep (map #(apply str (map
                                               (fn [x]
                                                 (cond
                                                   (= x 0) \o
                                                   (< x 10) x
                                                   (= x 10) \*
                                                   (= x 11) \+
                                                   (= x 15) \.
                                                   :else (str "(" x ")")))
                                               %)) (partition 10 barr)))))

;;;
;;; part 1
;;;

;(def arr (byte-array 100))
(def g (atom {}))

(defn do-all-flashes
  "For every 10 in data inc the adjacent levels.
  Afterwards all 10s will be changed into 15s.
  Note that levels that became 10 during this function call will not inc
  their adjacent levels yet. You will have to call this function again.
  Returns count of flashes."
  [data]
  ;; change all 10s to 11s:
  (swap-all-data! data #(if (= 10 %) 11 %))
  (let [flash-count (count (filter #{11} data))]
    ;; inc everything around a 11 (formerly 10):
    (doseq [row (range 10) ; all rows and cols
            col (range 10)]
      (when (= 11 (get-data data row col 0)) ; if flashing, then inc adjacent:
        (doseq [r (range (dec row) (+ row 2)) ; for all rows and cols...
                c (range (dec col) (+ col 2))] ; ...around the current flashing one...
          (when (and (or (not= r row) (not= c col)) ; only if: a) not the current one
                     (< (get-data data r c 99) 10)) ;   and: b) only if level < 10
            (swap-data! data r c inc)))
        ;; finally set the 11 (formerly 10) to 15 so it does not get processed again
        (set-data! data row col 15))) 
    flash-count))

(defn do-step
  [data]
  ;; first: inc every level by 1
  (swap-all-data! data inc)
  ;; then: all 10s flash influencing their neighbours
  ;; so: while there are 10s:
  (def fc (atom 0))
  (while (some #{10} data)
    ;; for all 10s do:
    ;; -> inc adjacent levels unless already 10
    ;; -> set the 10 to 11
    (swap! fc + (do-all-flashes data)))
  ;; finally: reset all flashed to 0
  ;; -> there must not be any 10s left
  (do-data data (fn [l] (when (and (>= l 10) (not= l 15)) (throw (ex-info (str "Wrong level " l) {})))))
  ;; -> reset all 15s to 0 
  (swap-all-data! data #(if (= % 15) 0 %))
  @fc)

(defn solve-part-1
  "Total of octopi flashes after 100 cycles."
  [data]
  (swap! g assoc :data (read-data data))
  { :total_flashes_after_100_steps (apply + (repeatedly 100 #(do-step (@g :data))))})


;;;
;;; part 2
;;;

(defn solve-part-2
  "todo"
  [data]
  (swap! g assoc :data (read-data data))
  (loop [i 1]
    (do-step (@g :data))
    ;(when (zero? (mod i 50)) (println i "..."))
    (if (and (some #{1 2 3 4 5 6 7 8 9} (@g :data)) (< i 1000))
      (recur (inc i))
      { :steps_for_total_sync i })))


;;;
;;; dev space
;;;
(comment
  (solve-part-1
    (.split (slurp "resources/day11_input_real.txt") "\\n"))

  (solve-part-2
    (.split (slurp "resources/day11_input_real.txt") "\\n"))

  (println (format-data (@g :data) "\n"))
  (swap-all-data! (@g :data) dec)
  (swap-all-data! (@g :data) inc)

  (def foo (byte-array 100))
  (def foo (aclone (@g :data)))
  (format-data foo)
  (doseq [i (range (alength foo))] (aset-byte foo i 0)) ; foo: set all 0
  (aset-byte foo 22 5)
  (aset-byte foo 55 9)

  (println (format-data foo "\n"))
  (apply + (repeatedly 100 #(do-step foo)))
)
