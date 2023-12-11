(ns adventofcode.dayeight)

(def input-string (slurp "input/8.txt"))

(def sample-string "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)")

(defn get-info [input]
  (let [[steps lines] (clojure.string/split input #"\n\n")]
    {
     :steps (cycle (seq steps))
     :maps (into {} (map (fn [line]
                           (let [[k lr-raw] (clojure.string/split line #"\s\=\s")
                                 [l r] (clojure.string/split (subs lr-raw 1 (- (count lr-raw) 1)) #"\,\s") ;cut off parens
                                 ]
                             [k {\L l \R r}]
                             )
                           ) (clojure.string/split-lines lines)))
     }
    )
  )

(defn count-steps-to-ZZZ [info]                             ; im really proud of this actually
  (loop
    [cur-key "AAA"
     next-steps (:steps info)
     steps-taken 0
     ]
    (if (= cur-key "ZZZ")
      steps-taken
      (recur (get (get (:maps info) cur-key) (first next-steps)) (rest next-steps) (inc steps-taken))
      )
    ))

(defn part-one-main [& args]
  (println (count-steps-to-ZZZ (get-info input-string)))
  )

;
; PART II
;

(defn get-info-p2 [input]
  (let [
        [steps lines] (clojure.string/split input #"\n\n")
        maps (into {} (map (fn [line]
                             (let [[k lr-raw] (clojure.string/split line #"\s\=\s")
                                   [l r] (clojure.string/split (subs lr-raw 1 (- (count lr-raw) 1)) #"\,\s") ;cut off parens
                                   ]
                               [k {\L l \R r}]
                               )
                             ) (clojure.string/split-lines lines)))
        ]
    {
     :steps (cycle (seq steps))
     :maps maps
     :starts-A (filter #(= \A (last (seq %))) (keys maps))
     }
    )
  )

(defn ends-Z? [k]
  (= \Z (last (seq k)))
  )

(defn count-steps-to-ends-Z [info start-key]                             ; im really proud of this actually
  (loop
    [cur-key start-key
     next-steps (:steps info)
     steps-taken 0
     ]
    (if (ends-Z? cur-key)
      steps-taken
      (recur (get (get (:maps info) cur-key) (first next-steps)) (rest next-steps) (inc steps-taken))
      )
    ))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b))
  )

(defn count-steps-to-all-ends-Z [info]                      ;with p2 info
  (reduce lcm (map #(count-steps-to-ends-Z info %) (:starts-A info)))
  )

(defn -main [& args]
  (println (count-steps-to-all-ends-Z (get-info-p2 input-string)))
  )