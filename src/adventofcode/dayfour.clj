(ns adventofcode.dayfour)

(defn matches-value [n]
  (if (= 0 n) 0
              (if (= 1 n) 1
                          (* 2 (matches-value (- n 1)))
                          )
              )
  )

(defn line-value [card]
  (count (filter (fn [have] (not-empty (filter (fn [win] (= win have)) (:win card)))) (:have card))) ; no longer functional - edited for Part II
  )

(defn card-info [line index]                                ;also present in Part II
  (let [[win have] (map #(map (fn [s] (Integer/parseInt s)) %)
                        (map #(clojure.string/split % #"\s+")
                             (clojure.string/split
                               (second (clojure.string/split line #"\:\s+"))
                               #"\s*\|\s*")))] {:index index :win win :have have})
  )

(defn line-values [lines] (reduce + (map #(line-value (card-info % -1)) lines))) ;retroactive add of "-1" ("index" param was not present in Part I)

(defn part-one-main [& args] (println (line-values (clojure.string/split-lines (slurp "input/4.txt")))))

;PART II

(defn card-output [card num-cards]
  (let [value (line-value card) index (:index card)] (seq (map (fn [i] (if (and (> i index) (<= i (+ index value ))) 1 0)) (range num-cards))))
  )

(defn card-outputs [input] (map-indexed #(card-output (card-info %2 %1) (count input)) input))

(defn state [num-collected cards] {:num-collected num-collected :cards (map (fn [card] {:card card :num 1}) cards)})

(defn next-state [curstate]
  {:num-collected
   (+ (:num (first (:cards curstate))) (:num-collected curstate))
   :cards
   (rest
     (let [allcards (:cards curstate) fir (first allcards)]
       (map-indexed
         (fn [i card] {
                       :card (rest (:card card))
                       :num (+ (* (nth (:card fir) i) (:num fir)) (:num card))})
         allcards)))
   }
  )

(defn recur-outputs [init-state]
  (loop [s init-state]
    (if (empty? (:cards s))
      s
      (recur (next-state s)))
    )
  )

(defn -main [& args]
  (println (:num-collected (recur-outputs (state 0 (card-outputs (clojure.string/split-lines (slurp "input/4.txt")))))))
  )