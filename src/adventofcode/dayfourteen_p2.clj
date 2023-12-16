(ns adventofcode.dayfourteen-p2)

(def sample-input "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#....")
(def input-string (slurp "input/14.txt"))

(defn inc-last [out]
  "increment the holds of the last element of out"
  (conj (vec (butlast out)) (into {} (assoc (last out) :holds (inc (:holds (last out))))))
  )

(defn static [y]
  {:y y :holds 0}
  )

(defn col->info [col]
  (loop [left col out [(static (count col))]]
    (if (empty? left)
      out
      (if (= (first left) \O)
        (recur (rest left) (inc-last out))
        (if (= (first left) \#)
          (recur (rest left) (assoc out (count out) (static (dec (count left)))))
          (recur (rest left) out)
          )
        )
      )
    )
)

(defn to-cols [string]
  (let [split (clojure.string/split-lines string)]
    (map #(mapv (fn [s] (nth s %)) split) (range (count (first split)))))
  )