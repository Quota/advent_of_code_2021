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
  #_ (apply println "[LOG]" xs))
