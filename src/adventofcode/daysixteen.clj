(ns adventofcode.daysixteen)

(def input-string (slurp "input/16.txt"))
(def sample-string ".|...\\....\n|.-.\\.....\n.....|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|....")

(defn grid [grid-string]
  (clojure.string/split-lines grid-string)
  )

(defn grid-get [x y g]
  (nth (nth g y []) x nil)
  )

(defn energize [x0 y0 dx0 dy0 g]
  (loop [x x0, y y0, dx dx0, dy dy0, out '(), hit #{}, q '()] ; the queue holds the remaining beams to run, basically deferred recursion (for stack)
    (let [c (grid-get x y g)]
      (if (or (contains? hit [x y dx dy]) (nil? c))
        (if (empty? q)
          out
          (let [{nx :x ny :y ndx :dx ndy :dy} (first q)]
            (recur nx ny ndx ndy out (conj hit [x y dx dy]) (rest q))
            )
          )
        (if (or (= c \.) (and (= c \-) (= dy 0)) (and (= c \|) (= dx 0)))
          (recur (+ x dx) (+ y dy) dx dy (conj out [x y]) (conj hit [x y dx dy]) q)
          (if (= c \-)
            (recur (dec x) y -1 0 (conj out [x y]) (conj hit [x y dx dy]) (conj q {:x (inc x) :y y :dx 1 :dy 0}))
            (if (= c \|)
              (recur x (dec y) 0 -1 (conj out [x y]) (conj hit [x y dx dy]) (conj q {:x x :y (inc y) :dx 0 :dy 1}))
              (if (= c \\)
                (recur (+ x dy) (+ y dx) dy dx (conj out [x y]), (conj hit [x y dx dy]) q)
                (recur (- x dy) (- y dx) (- dy) (- dx) (conj out [x y]) (conj hit [x y dx dy]) q)
                )
              )
            )
          )
        )
      )
    )
  )

(defn num-energized [x y dx dy g]
  (count (set (energize x y dx dy g)))                      ; remove duplicates
  )

(defn part-one-main [& args]
  (println (num-energized 0 0 1 0 (grid input-string)))
  )

;
; PART II
;

(defn -main [& args]
  (reduce max (let [g (grid input-string), mx (dec (count g))] ;always square
                (map #(reduce max
                              [
                               (num-energized 0 % 1 0 g)
                               (num-energized mx % -1 0 g)
                               (num-energized % 0 0 1 g)
                               (num-energized % mx 0 -1 g)]
                              ) (range mx))))
  )