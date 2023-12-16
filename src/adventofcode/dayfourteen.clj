(ns adventofcode.dayfourteen)

(def sample-input "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#....")
(def input-string (slurp "input/14.txt"))

(defn to-cols [string]
  (let [split (clojure.string/split-lines string)]
    (map #(mapv (fn [s] (nth s %)) split) (range (count (first split)))))
  )

(defn score [info]
  (reduce + (map (fn [{holds :holds y :y}] (/ (* holds (+ y (- y holds -1))) 2)) info))
  )

(defn inc-last [out]
  "increment the holds of the last element of out"
  (conj (vec (butlast out)) (into {} (assoc (last out) :holds (inc (:holds (last out))))))
  )

(defn static [y]
  {:y y :holds 0}
  )

(defn col->info [col]
  (filter #(pos? (:holds %)) (loop [left col out [(static (count col))]]
                               (if (empty? left)
                                 out
                                 (if (= (first left) \O)
                                   (recur (rest left) (inc-last out))
                                   (if (< (count left) 2)
                                     out
                                     (if (and (not= (second left) \#) (= (first left) \#))
                                       (recur (rest left) (assoc out (count out) (static (dec (count left)))))
                                       (recur (rest left) out)
                                       )
                                     )
                                   )
                                 )
                               ))
  )

(defn -main [& args]
  (println (reduce + (map score (map col->info (to-cols input-string)))))
  )