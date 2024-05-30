(ns adventofcode.daytwelve)

(def sample-string "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1")
(def input-string (slurp "input/12.txt"))

(defn get-records [string]
  (map (fn [line]
         (let [[record groups-string]
               (clojure.string/split line #"\s")
               groups
               (map #(Integer/parseInt %) (clojure.string/split groups-string #"\,"))]
           {:record (seq record) :groups groups}
           ))
       (clojure.string/split-lines string))
  )

(defn get-groups [known-seq]
  (loop [left known-seq latest \. now 0 groups []]
    (if (empty? left)
      (if (zero? now)
        groups
        (conj groups now)
        )
      (if (= \. (first left))
        (if (= latest \#)
          (recur (rest left) (first left) 0 (conj groups now))
          (recur (rest left) (first left) 0 groups)
          )
        (recur (rest left) (first left) (inc now) groups)
        )
      )
    ))

(defn has-unknown? [record-seq]
  (some #(= \? %) record-seq)
  )

(defn replace-first-unknown [record-seq c]
  (concat (take-while #(not= % \?) record-seq) (list c) (rest (drop-while #(not= % \?) record-seq)))
  )

(defn count-arrangements [rseq groups]
  (if (not (has-unknown? rseq))
    (if (= (get-groups rseq) groups)
      1
      0
      )
    (+
      (count-arrangements (replace-first-unknown rseq \.) groups)
      (count-arrangements (replace-first-unknown rseq \#) groups)
      )
    )
  )

;my ass is NOT doing part ii
(defn -main [& args]                                        ; not super fast but pmap is cool
  (println (reduce +
                   (pmap #(let [{record :record groups :groups} %]
                            (count-arrangements record groups))
                         (get-records input-string))))
  )