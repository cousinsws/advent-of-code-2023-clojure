(ns adventofcode.dayseven-optim)

;(part 2 optim)

(def sample-input-string "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483")

(def full-input-string (slurp "input/7.txt"))

(defn total-winnings [sorted-hands]
  (reduce +(map-indexed (fn [i hand] (* (:bid hand) (inc i))) sorted-hands))
  )

(def deck
  (apply hash-map (reduce concat (map-indexed (fn [id ch] [ch id]) [\J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A])))
  )

(defn get-hands [input-string]
  (map (fn [line]
         (let [[hand bid] (clojure.string/split line #"\s+")]
           {
            :hand (map #(get deck %) (seq hand))
            :bid (Integer/parseInt bid)
            }
           )
         ) (clojure.string/split-lines input-string))
  )

(defn priority-compare [elem1 elem2 & comp-funs]
  "Compare two elements according to a number of ranked comparison functions - if one
  is zero, the next is used for the comparison, and so on."
  (loop
    [funs comp-funs]
    (let [
          comp1 ((first funs) elem1 elem2)
          ]
      (if (and (> (count funs) 1) (= 0 comp1)) (recur (rest funs)) comp1) ; if we reach the end and it's still tied, then the whole thing is tied
      )
    )
  )

(defn lexical-compare [coll1 coll2]
  (loop
    [coll1-left coll1 coll2-left coll2]
    ( if (and (= 0 (count coll1-left) (count coll2-left)))
      0
      ( let [comparison (compare (first coll1-left) (first coll2-left))]
        (if (= 0 comparison)
          (recur (rest coll1-left) (rest coll2-left))
          comparison
          )))
    )
  )

(defn frequency-class [coll]
  (sort #(- (compare %1 %2)) (vals (frequencies coll)))
  )

(defn replace-jokers [hand]
  (let [with
        (nth (drop-while zero? (map first (sort #(- (compare (second %1) (second %2))) (frequencies hand)))) 0 1) ; the first (or, if none, 1) non-joker in the sorted frequencies list
        ]
    (map #(if (zero? %) with %) hand)                       ; replace
  )
)

(defn sort-by-hands [hands]
  (sort
    (fn [elem1 elem2]
          (priority-compare elem1 elem2
            #(lexical-compare                               ; primary comparison
               (frequency-class (replace-jokers %1))
               (frequency-class (replace-jokers %2)))
            lexical-compare        ; secondary comparison
           ))
    (map #(:hand %) hands)
  )
)

(defn -main [& args]
  (println (total-winnings (sort-by-hands (get-hands full-input-string))))
  )
