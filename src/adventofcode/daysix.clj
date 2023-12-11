(ns adventofcode.daysix)

(defn new-race [time distance]
  {:time time :distance distance}
  )

(defn list-fn-p1 [line]
  (map #(Integer/parseInt %) (clojure.string/split (second (clojure.string/split line #"\:\s+")) #"\s+"))
  )

(defn races-from-input [full-string]
  (let [
        lines (clojure.string/split-lines full-string)
        times (list-fn-p1 (first lines))
        distances (list-fn-p1 (second lines))
        ]
    (map (fn [time distance] (new-race time distance)) times distances)
    )
  )

(defn get-distance [time-charged time-travel]
  (* time-charged time-travel)
  )

(defn distance-with [total-time time-charged]
  (get-distance time-charged (- total-time time-charged))
  )

(defn possible-distances [total-time]
  (map (fn [time-charged] (distance-with total-time time-charged)) (take (+ total-time 1) (range)))
  )

(defn successes-part1 [total-time d-to-beat]
  (filter (fn [d-made] (> d-made d-to-beat)) (possible-distances total-time))
  )

(defn successes-per-race [races-list]
  (map (fn [{time :time distance :distance}] (successes-part1 time distance)) races-list)
  )

(defn part-one-main [& args]
  (println (reduce * (map count (successes-per-race (races-from-input (slurp "input/6.txt"))))))
  )

;
; PART II
;

(defn successes [{total-time :time d-to-beat :distance}]
  (filter (fn [d-made] (> d-made d-to-beat)) (possible-distances total-time))
  )

(defn list-fn [line]
  (Long/parseLong (clojure.string/replace (second (clojure.string/split line #"\:\s+")) #"\s+" ""))
  )

(defn race-from-input [full-string]
  (let [
        lines (clojure.string/split-lines full-string)
        time (list-fn (first lines))
        distance (list-fn (second lines))
        ]
    (new-race time distance)
    )
  )

(defn -main [& args]
  (println (count (successes (race-from-input (slurp "input/6.txt"))))) ; brute force
  )