(ns adventofcode.dayeleven)

(def input-string (slurp "input/11.txt"))

(def sample-string "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#.....")

(defn pos [x y]
  [x y]
  )

(defn x [p]
  (first p)
  )

(defn y [p]
  (second p)
  )

(defn string->grid [string]
  (reduce concat
          (filter first
                  (map-indexed
                    (fn [y line]
                      (filter identity
                          (map-indexed
                              (fn [x c] (if (= c \#) (pos x y) nil))
                            line)))
                    (clojure.string/split-lines string))))
  )

(defn empty-in-grid [g]
  ( let [mx (inc (reduce max (map first g))) my (inc (reduce max (map second g)))]
    {
     :columns (filter (fn [i] (empty? (filter #(= i (first %)) g))) (range mx))
     :rows (filter (fn [i] (empty? (filter #(= i (second %)) g))) (range my))
     })
  )

(defn count-below [n coll]
  (count (filter (partial >= n) coll))
  )

(defn expand [g]
  (let [{empty-cols :columns empty-rows :rows} (empty-in-grid g)]
    (map (fn [[px py]]
           (pos (+ px (count-below px empty-cols)) (+ py (count-below py empty-rows)))
           ) g)
    )
  )

(defn distance [p1 p2]
  (+ (abs (- (x p1) (x p2))) (abs (- (y p1) (y p2))))
  )

(defn sum-distances [g]
  (reduce + (map-indexed (fn [i p] (reduce + (map #(distance p %) (drop (inc i) g)))) g))
  )

(defn expand-large [g factor]
  (let [{empty-cols :columns empty-rows :rows} (empty-in-grid g)]
    (map (fn [[px py]]
           (pos (+ px (* factor (count-below px empty-cols))) (+ py (* factor (count-below py empty-rows))))
           ) g)
    )
  )

(defn part-one-main [& args]
  (sum-distances (expand (string->grid input-string)))
  )

; part two
(defn -main [& args]
  (println (sum-distances (expand-large (string->grid input-string) 999999)))
  )