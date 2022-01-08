(ns aoc2021.core
  (:gen-class)
  (:require [clojure.pprint :as pp]))

(defn usage
  []
  (str "Usage: (-main <day> <part> [data])\n"
       "       <day>      day of which to run the solution\n"
       "       <part>     part of day of which to run the solution\n"
       "       [data]     input data for <day>. if missing read from stdin."))

(defn -main
  [& args]
  (if (or (< (count args) 2)
          (> (count args) 3))
    (println (usage))
    (let [day (first args)
          part (second args)
          data (or (nnext args) (line-seq (java.io.BufferedReader. *in*)))
          day-with-0 (if (= (.length day) 1) (str "0" day) day)
          _ (load (str "/aoc2021/day" day-with-0))
          day-part-fn (ns-resolve (symbol (str "aoc2021.day" day-with-0))
                                  (symbol (str "solve-part-" part)))
          result (day-part-fn data)]
      (pp/pprint result))))
