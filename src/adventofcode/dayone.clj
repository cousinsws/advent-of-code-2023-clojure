(ns adventofcode.dayone)

(def input "input/1-1.txt")

(defn foo [] (println (subs (slurp input) 0 40)))

(def numbers {
               "one" 1
               "two" 2
               "three" 3
               "four" 4
               "five" 5
               "six" 6
               "seven" 7
               "eight" 8
               "nine" 9
               })

(defn first-digit [string]
  (second (re-find #"(\d|one|two|three|four|five|six|seven|eight|nine).*" string))
  )

(defn last-digit [string]
  (second (re-matches #".*(\d|one|two|three|four|five|six|seven|eight|nine)[a-zA-Z]*$" string))
  )

(defn value [in]
  (if (every? #(Character/isDigit %) in)
    (Integer/parseInt in)
    (get numbers in)
    )
  )

(defn line-value [string]
  (+ (* 10 (value (first-digit string))) (value (last-digit string)))
  )

(defn strings [inputfull]
  (clojure.string/split-lines inputfull)
  )

(defn sum-values [inputfull]
  (reduce
    (fn [out line] (+ out (line-value line)))
    0 (strings inputfull))
  )

(defn -main [& args] (println (sum-values (slurp input))))