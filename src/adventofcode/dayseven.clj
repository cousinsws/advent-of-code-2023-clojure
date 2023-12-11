(ns adventofcode.dayseven)

(def deck
  (apply hash-map (reduce concat (map-indexed (fn [id ch] [ch id]) [\J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A])))
  )

(defn value [card]
  (get deck card)
  )

(defn get-hands [input-string]
  (map (fn [line]
         (let [[hand bid] (clojure.string/split line #"\s+")]
           {
            :hand (map value (seq hand))
            :bid (Integer/parseInt bid)
            }
           )
         ) (clojure.string/split-lines input-string))
  )

(def hand-score-freq
  (apply hash-map (reduce concat (map-indexed (fn [id ch] [ch id]) [
      [1 1 1 1 1]                                           ; no pairs
      [1 1 1 2]                                             ; one pair
      [1 2 2]                                               ; two pair
      [1 1 3]                                               ; three of a kind
      [2 3]                                                 ; full house
      [1 4]                                                 ; four of a kind
      [5]])))                                               ; five of a kind
  )

(defn score-hand-p1 [hand]
  (get hand-score-freq (first (filter (partial = (sort (vals (frequencies hand)))) (keys hand-score-freq))))
  )

(defn pow [x n]
  (reduce * (repeat n x)))

(defn tiebreak-score [hand]
  (reduce + (map-indexed #(* (pow 15 (- 4 %1)) %2) hand))
  )

(defn compare-tiebreak [hand1 hand2]
  (compare (tiebreak-score hand1) (tiebreak-score hand2))
  )

(defn sort-by-hands-p1 [hands]
  (let [primary-sort
        (sort-by :hand-score (map #(assoc % :hand-score (score-hand-p1 (:hand %))) hands))
        ]
    (loop [hands-left primary-sort
           primary-score 0
           sorted-hands []]
      (if (empty? hands-left)
        sorted-hands
        (recur
          (drop-while #(= primary-score (:hand-score %)) hands-left)
          (inc primary-score)
          (concat sorted-hands (sort #(compare-tiebreak (:hand %1) (:hand %2)) (take-while #(= primary-score (:hand-score %)) hands-left)))
          )
        )
      )
    )
  )

(defn total-winnings [sorted-hands]
  (reduce +(map-indexed (fn [i hand] (* (:bid hand) (inc i))) sorted-hands))
  )

(def sample-input-string "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483")

(def full-input-string (slurp "input/7.txt"))

(defn part-one-main [& args]
  (println (total-winnings (sort-by-hands-p1 (get-hands full-input-string))))
  )

(defn score-hand-raw [hand]
  (get hand-score-freq (first (filter (partial = (sort (vals (frequencies hand)))) (keys hand-score-freq))))
  )

(defn score-hand [hand]
  (score-hand-raw (let [
                        hand-j-first (sort hand)
                        freq (sort #(- (compare (second %1) (second %2))) (frequencies hand))
                        num-j (get (into {} freq) 0 0)
                        last-val (first (drop-while zero? (keys freq)))
                        ]
                    (concat
                      (repeat num-j (if (nil? last-val) 1 last-val))
                      (drop-while zero? hand-j-first)
                      )
                    ))
  )

(defn sort-by-hands [hands]
  (let [primary-sort
        (sort-by :hand-score (map #(assoc % :hand-score (score-hand (:hand %))) hands))
        ]
    (loop [hands-left primary-sort
           primary-score 0
           sorted-hands []]
      (if (empty? hands-left)
        sorted-hands
        (recur
          (drop-while #(= primary-score (:hand-score %)) hands-left)
          (inc primary-score)
          (concat sorted-hands (sort #(compare-tiebreak (:hand %1) (:hand %2)) (take-while #(= primary-score (:hand-score %)) hands-left)))
          )
        )
      )
    )
  )

(defn -main [& args]
  (println (total-winnings (sort-by-hands (get-hands full-input-string))))
  )