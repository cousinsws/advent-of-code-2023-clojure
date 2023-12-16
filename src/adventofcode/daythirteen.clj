(ns adventofcode.daythirteen)

(def input-string (slurp "input/13.txt"))
(def sample-string "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.\n\n#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#")

(defn grid [grid-string]
  (clojure.string/split-lines grid-string)
  )

(defn all-grids [input]
  (map grid (clojure.string/split input #"\n\n"))
  )

(defn get-row [g n]
  (nth g n nil)
  )

(defn get-col [g n]
  (let [s (apply str (reduce conj [] (map #(nth % n nil) g)))] (if (empty? s) nil s))
  )

(defn split-grid [g get-fn n]
  "splits before the nth row/col and returns a list of two colls, with the first = reverse(before), second = after."
  (
    let [tick (atom 0) all (repeatedly #(swap! tick inc))]
    (list
      (reverse (reduce conj [] (map (partial get-fn g) (range n))))
      (seq (reduce conj [] (map (partial get-fn g) (take-while #(identity (get-fn g %)) (drop (dec n) all))))) ; seq it for consistency
      )
    )
  )

(defn is-mirror [[below above]]
  (let [snip (filter #(= (count %) 2) (map vector below above))]
    (if (every? #(= (first %) (second %)) snip) (count below) 0)
    )
  )

(defn len [g get-fn]
  (if (= get-fn get-row)
    (count g)
    (count (first g))
    )
  )

(defn get-mirror [g get-fn]
  (let [l (len g get-fn)] (apply max (map #(is-mirror (split-grid g get-fn %)) (range l))))
  )

(defn mirror-val [g]
  (+ (* 100 (get-mirror g get-row)) (get-mirror g get-col))
  )

(defn -main [& args]
  (println (reduce + (map mirror-val (all-grids input-string))))
  )

(defn smudges [g]
  (let [orig (mirror-val g)]
    (for [y (range (len g get-row)) x (range (len g get-col))]
      (let [cur (nth (nth g y) x)
            to (if (= cur \.) \# \.)]
        {:x x :y y :grid (assoc g y (apply str (assoc (vec (get-row g y)) x to)))}
        )
      )))