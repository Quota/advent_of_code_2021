(ns aoc2021.day04
  (:require [clojure.string :as str]
            [aoc2021.util :as u]))

;;;
;;; part 1
;;;

(defn read-board-row
  "Input: 
    11 12 13 14 15
   Output:
    [#{11} #{12} #{13} #{14} #{15}]"
  [row-data]
  (->> row-data
       (str/trim) ; remove whitespaces
       (#(str/split % #" +")) ; split at (multiple) spaces
       (map (comp set list)) ; map every string to a set containing that string
       vec)) ; put all the maps into one vector

(defn read-board
  "Input:
    <empty line>
     1  2  3  4  5
     6  7  8  9 10
    11 12 13 14 15
    16 17 18 19 20
    21 22 23 24 25
  Output:
    {:board 
      [[#{1} #{2} #{3} #{4} #{5}]
       [#{6} #{7} #{8} #{9} #{10}]
       [#{11} #{12} #{13} #{14} #{15}]
       [#{16} #{17} #{18} #{19} #{20}]
       [#{21} #{22} #{23} #{24} #{25}]]
     :data
      <seq pointing the next empty line to process, or empty> }"
  [data]
  (loop [current-data (rest data)
         board []
         row-num 0]
    (if (empty? (first current-data)) ; check for empty line
      {:board board :data current-data}
      (recur (rest current-data)
             (conj board (read-board-row (first current-data)))
             (inc row-num)))))

(defn read-boards
  "Input:
   <empty line>
   <board data>
   ...<more empty line + board data>...
  Output:
   Set of board maps.
  For more concrete examples see `read-board`."
  [data]
  (loop [current-data data
         boards #{}]
    (if (empty? current-data)
      boards
      (let [result (read-board current-data)]
        (recur (:data result)
               (conj boards (:board result)))))))

(defn play-board
  "Add :mk to all cells (which are of type set) which contain number."
  [board number]
  (clojure.walk/prewalk
    (fn [node] (if (and (set? node)
                        (get node number))
                 (conj node :mk)
                 node))
    board))

(defn play-boards
  "Call `play-board <board> number` for all given boards."
  [boards number]
  (map #(play-board % number) boards))

(defn check-win-col
  "Check if given column <col> wins and return it, otherwise return nil.
  Input: <board> <col>
  Output: nil | <n>"
  [board col]
  (when (every? (fn [cell] (:mk cell))
                (for [row (range (count board))]
                  (get-in board [row col])))
    col))

(defn check-win
  "Check if the board wins and return infos about the win condition.
  Otherwise return nil.
  Input: <board>
  Output: nil | {:board <board> :row <n>} | {:board <board> :col <n>}"
  [board]
  (or
    ;; horizontal check: iterate over rows and check every row vector for :mk's
    (loop [rows board
           i 0]
      (let [row (first rows)]
        (cond
          ; if every?... then the row wins -> return it:
          (every? #(:mk %) row) {:board board :row i}
          ; loop to next row if we have more:
          (seq (rest rows)) (recur (rest rows) (inc i))
          ; nil if all rows exhausted and no winner:
          :else nil)))
    ;; vertical check: iterate over columns calling check-win-col for every
    ; thread with column index (0 .. <num of cols - 1>)
    (->> (range (count (first board))) 
         ; ...only keep column that win
         (filter #(check-win-col board %))
         ; ...convert winning column index into useful result data
         (map #(assoc {:board board} :col %))
         ; ...only want the first winner if any, or nil
         first)))

(defn check-win-boards
  "Input: [<board> ...]
  Output: () | ({:board <board> :row <n>}...) | ({:board <board> :col <n>}...)"
  [boards]
  (filter some? (map check-win boards)))

(defn play-first-winner
  "Input:
  <seq of numbers>
  <seq of boards>
  Output:
  {:winners
   ({:board <board>
     :row <n>
     :won-with <n>} todo
    ...)
   :result
   {:last-num <current number>
    :boards
     (<all boards with marks up to the current number)}}"
  [boards all-numbers]
  (loop [numbers all-numbers
         result {:last-num nil :boards boards}]
    (let [winner (->> (:boards result)
                      check-win-boards
                      (map #(assoc % :won-with (:last-num result)))
                      seq)]
      (if (or winner (empty? numbers))
        {:winners winner
         :result result}
        (recur (rest numbers)
               {:last-num (first numbers)
                :boards (play-boards (:boards result)
                                     (first numbers))})))))

(defn calc-unmarked-sum
  "Calculate sum of all cells that are not marked with :mk."
  [board]
  (->> board
       (mapcat identity)
       (filter #(not (:mk %)))
       (mapcat identity)
       (map #(Integer/parseInt %))
       (reduce +)))

(defn add-board-score
  "To the given winner data add the unmarked-sum and score."
  [winner-record]
  (let [unmarked-sum (calc-unmarked-sum (:board winner-record))]
    (assoc winner-record
           :unmarked-sum unmarked-sum
           :score (* unmarked-sum (Integer/parseInt (:won-with winner-record))))))

(defn add-board-scores
  "Adds scores to all :winners of given result. 
  Actually simply applies `add-board-score` to all :winners.
  Input:
  <result of `play-first-winner`>
  Output:
  Result as above with `:unmarked-sum <n> :score <n>` added to :winners."
  [result]
  {:winners (map add-board-score (:winners result))
   :result (:result result)})

(defn solve-part-1
  "bingo, find first winner"
  [data]
  (let [numbers (str/split (first data) #",")
        boards (read-boards (rest data))]
    (add-board-scores (play-first-winner boards numbers))))


;;;
;;; part 2
;;;

(defn play-last-winner
  "Input:
  <seq of numbers>
  <seq of boards>
  Output:
  {:winners
   ({:board <board that won first>
     :row <n>
     :won-with <n>
     :unmarked-sum <n>
     :score <n>})
    ...
    {:board <board that won last>
     :row <n>
     :won-with <n>
     :unmarked-sum <n>
     :score <n>})
   :result
   {:last-num <current number>
    :boards (...<probably empty>...)}}"
  [boards all-numbers]
  (loop [numbers all-numbers
         result {:last-num nil :boards boards :winners '()}]
    (if (or (empty? numbers) (empty? (:boards result)))
      ; finish, return winners and remaining (i.e. empty) result
      {:winners (:winners result)
       :result (dissoc result :winners)}
      ; otherwise calc next step and recur...
      (let [next-number (first numbers)
            next-boards (play-boards (:boards result) next-number)
            next-winners (->> next-boards
                              check-win-boards
                              (map #(assoc % :won-with next-number))
                              seq)
            next-boards-w-o-winners (reduce (fn [boards winner]
                                              (remove #(= % (:board winner)) boards))
                                            next-boards
                                            next-winners)]
        (recur (rest numbers)
               {:last-num next-number
                :boards next-boards-w-o-winners
                :winners (concat (:winners result) next-winners)})))))

(defn solve-part-2
  "bingo, calc all winners"
  [data]
  (let [numbers (str/split (first data) #",")
        boards (read-boards (rest data))]
    (add-board-scores (play-last-winner boards numbers))))
