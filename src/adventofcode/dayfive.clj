(ns adventofcode.dayfive)


(defn map-fn [dest-range src-range len]
  (fn [in]
    (if (and (>= in src-range) (< (- in src-range) len))
      (+ dest-range (- in src-range))
      nil
      )
    )
  )

(defn to-map-fn [line-str]
  (apply map-fn (map #(Long/parseLong %) (clojure.string/split line-str #"\s+")))
  )

(defn to-map-fns [section-str]
  (map to-map-fn (clojure.string/split-lines section-str))
  )

(defn to-map-sets [section-strs]
  (map to-map-fns section-strs)
  )

(defn input-to-info [full-input-str]
  (let [split-str (seq (clojure.string/split full-input-str #"\n\n[a-z\-]*\smap\:\n"))]
    {
     :seeds (map #(Long/parseLong %) (clojure.string/split (second (clojure.string/split (first split-str) #"\: ")) #"\s+"))
     :map-sets (to-map-sets (rest split-str))
     }
    )
  )

(defn next-id [cur-id map-set]
  (let [
        ids-mapped (map #(% cur-id) map-set)
        found-id (first (drop-while #(not (identity %)) ids-mapped))
        ]
    (if found-id found-id cur-id)
    )
  )

(defn seeds-to-locs [info]
  (map (fn [seed] (
                    loop [cur-id seed maps-left (:map-sets info)]
                    (
                      if (empty? maps-left)
                          cur-id
                          (recur
                            (next-id cur-id (first maps-left))
                            (rest maps-left)
                            )
                          )

                    )) (:seeds info))
  )

(defn part-one-main [& args]
  (println (reduce min (seeds-to-locs (input-to-info (slurp "input/5.txt")))))
  )

;
; PART II
;

(defn get-range [start end]
  {:start start :end end}
  )

(defn to-map [line-str]
  (let [[dest-range src-range len] (map #(Long/parseLong %) (clojure.string/split line-str #"\s+"))]
    {:dest (get-range dest-range (+ dest-range len)) :src (get-range src-range (+ src-range len))}
    )
  )

(defn to-maps [section-str]
  (map to-map (clojure.string/split-lines section-str))
  )

(defn to-map-sets [section-strs]
  (map to-maps section-strs)
  )

(defn input-to-info [full-input-str]
  (let [split-str (seq (clojure.string/split full-input-str #"\n\n[a-z\-]*\smap\:\n"))]
    {
     :seed-fn (map
                (fn [[start len]] {:start start :end (+ start len)})
                (vec (apply hash-map (map #(Long/parseLong %) (clojure.string/split (second (clojure.string/split (first split-str) #"\: ")) #"\s+")))))
     :map-sets (to-map-sets (rest split-str))
     }
    )
  )

