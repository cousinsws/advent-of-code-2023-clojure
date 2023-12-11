(ns adventofcode.daytwo)

(def file "input/2.txt")

(defn get-input [in] (clojure.string/split-lines in))

;greater than - fail
(def cap {
          "red" 12
          "green" 13
          "blue" 14
          })

(defn over-cap? [n color]
  (> (Integer/parseInt n) (get cap color))
  )

(defn games-from [in] (map #(
                   let [[k v] (clojure.string/split % #": ")]
                   (clojure.string/split v #"[,;]\s")

                   ) in))

(defn fails? [throws]
  (
    not-empty (filter #(apply over-cap? (clojure.string/split % #"\s")) throws))
  )

(defn sum-success-ids [games]
  (reduce + (map-indexed #(
                            if (fails? %2) 0 (+ 1 %1)
                                           ) games))
  )

;archive
(defn part-one-main [& args]
  (println (sum-success-ids (games-from (get-input (slurp file)))))
  )

;
; PART II
;

(defn is-color [color]
  (fn [pull]
    (
      clojure.string/includes? pull color
                               )
    )
  )

(defn get-power [red green blue]
  (* red green blue)
  )

(defn get-value [pull]
  (Integer/parseInt (re-find #"\d+" pull))
  )

(defn get-min [game color]
  (apply max (map get-value (filter (is-color color) game)))
  )

(defn color-min [color games] (map #(get-min % color) games))

(defn game-powers [games] (mapv get-power (color-min "red" games) (color-min "green" games) (color-min "blue" games)))

(defn sum-game-powers [games] (reduce + (game-powers games)))

(defn -main [& args]
  (println (sum-game-powers (games-from (get-input (slurp file)))))
  )