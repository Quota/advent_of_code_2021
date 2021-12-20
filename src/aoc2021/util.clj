(ns aoc2021.util)

(comment
  (defn- pln
    [lvl msg]
    ; prepend #_ to comment out
    (apply 
      println 
      (case lvl
        :log "[LOG]"
        :info "[INF]"
        :warn "[WRN]"
        :error "[ERR]"
        (str "[" lvl "]"))
      msg)))

(defn log
  [& xs]
  ;(pln :log xs)
  ; prepend #_ to comment out
  #_(apply println "[LOG]" xs))

(defn binary-to-number
  "Gets a list of bits (0 and 1) and returns the number these bits describe."
  ([bits] (binary-to-number bits 0))
  ([bits number]
   (if (empty? bits)
     number
     (recur (rest bits)
            (-> number
                (* 2)
                (+ (first bits)))))))

(defn parse-input-numbers
  "Input: (\"n,n,...\") ; list with one string with numbers separated by comma
  Output: (n n ...) ; list of integers"
  [lines]
  (as-> lines $
    (first $)
    (clojure.string/split $ #"[, ]+")
    (map #(Integer/parseInt %) $)))

