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

(defn parse-digits
  "Input: \"nnn...\" ; string with digits
  Output: (n n ...) ; list of integers"
  [digits]
  (map #(- (int %) 48) (seq digits)))

(defn parse-numbers
  "Input: (\"n...n...\" \"...\") ; list with strings of integers
  Output: ((n n ...) ...) ; list of list of numbers
  Argument `sep` is the regex to use to split numbers within strings, default is spaces (one or more)"
  ([in]
   (parse-numbers #" +" in))
  ([sep in]
   (->> in
        (map #(clojure.string/split % sep))
        (map (fn[x] (map #(Integer/parseInt %) x))))))
