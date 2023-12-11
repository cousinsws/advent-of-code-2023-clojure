(ns adventofcode.daythree)

(defn get-input [input-string] (clojure.string/split-lines input-string))

(defn neighborhood [[before line after] start len]
  (let [end (+ start len)]
    [
     (subs before start end)
     (subs line start end)
     (subs after start end)
     ]
    )
  )

(defn is-symbol? [x] (not (or (Character/isDigit x) (= x \.))))

(defn first-number [line] (
            apply str (take-while #(Character/isDigit %) (seq line))))

(defn pad-empty [lines] (
          let [pad (apply str (repeat (+ 2 (count (first lines))) "."))]
          (vec (
             cons
                 pad
                 (conj
                   (vec (map #(str "." % ".") lines))
                   pad)
             )
          )))


(defn chars-left-line [line] (
           map
             #(count (first-number (subs line %)))
             (range (count line))
 ))

(defn digit-spots [line] (
       filter #(not (nil? %))
              (let [cll (chars-left-line line)]
                (map-indexed
                            #(if (<=
                                    (nth cll %1)
                                    (if (= %1 0)
                                          0
                                          (nth cll (- %1 1)))
                                    )
                                nil
                                {:index %1 :len %2}
                                )
                             cll))
              )
  )

(defn spots->neighborhoods [spots line-index lines]
  (map #(neighborhood (subvec lines (- line-index 1) (+ line-index 2)) (- (:index %) 1) (+ (:len %) 2)) spots)
  )

(defn real-neighborhoods [dspots lines]
  (map-indexed #(if (or (= %1 0) (= %1 (- (count dspots) 1))) '() (spots->neighborhoods %2 %1 lines)) dspots)
  )

(defn real-nh-from-lines [lines]
  (real-neighborhoods (map digit-spots lines) lines)
  )

(defn neighborhood-has-symbol? [neighborhood]
  (let [full-neighborhood (apply str neighborhood)]
    (not-empty (filter is-symbol? (seq full-neighborhood)))
    )
  )

(defn neighborhood-value [hood]
  (let [s (nth hood 1)] (Integer/parseInt (subs s 1 (- (count s) 1))))
  )

;archive
(defn part-one-main [& args]
  (println (
     reduce +
            (map neighborhood-value
                 (filter neighborhood-has-symbol?
                         (reduce concat
                                 (real-nh-from-lines (pad-empty (get-input (slurp "input/3.txt"))))
                         )
                 )
             )
  ))
  )

;
; PART II
;

(defn spot->neighborhood [spot line-index lines]
  (neighborhood (subvec lines (- line-index 1) (+ line-index 2)) (- (:index spot) 1) (+ (:len spot) 2))
  )

(defn gear-spot-line [line-str line-index index]
  (filter #(not (nil? %)) (map-indexed #(if (= \* %2) {:line-index line-index :index (+ index %1)} nil) (seq line-str)))
  )

(defn attach-lines [dspot-arr]
  (reduce concat (map-indexed (fn [list-i dlist] (map #(assoc % :line-index list-i) dlist)) dspot-arr))
  )

(defn gear-spot [neighborhood line-index index]
  (apply concat (map-indexed #(gear-spot-line %2 (+ line-index %1 -1) index) neighborhood))
  )

(defn all-info [lines]
  (let [dspots (attach-lines (map digit-spots lines))]
    (map #(let [hood (spot->neighborhood % (:line-index %) lines)]
            (assoc % :value (neighborhood-value hood) :gears (gear-spot hood (:line-index %) (:index %)))
            ) dspots)
    )
  )

(defn all-gears [info]
  (reduce concat (filter not-empty (map #(:gears %) info)))
  )

(defn has-gear [gear single-info]
  (not-empty (filter #(= % gear) (:gears single-info)))
  )

(defn gears-with-parts [lines]
  (let [info (all-info lines) gears (set (all-gears info))]
    (map (fn [gear]
           (filter (partial has-gear gear) info)
           ) gears)
    )
  )

(defn gear-ratio [parts]
  (reduce * (map #(:value %) parts))
  )

(defn -main [& args]
  (println (reduce + (map gear-ratio (filter #(= (count %) 2) (gears-with-parts (pad-empty (get-input (slurp "input/3.txt"))))))))
  )
