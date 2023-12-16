(ns adventofcode.dayfifteen)

(def sample-string "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
(def input-string (slurp "input/15.txt"))
(defn split [s]
  (clojure.string/split s #",")
  )

(defn hashf [s]
  (loop [left (seq s) cur 0]
    (if (empty? left)
      cur
      (recur (rest left) (mod (* (+ cur (int (first left))) 17) 256))
      )
    )
  )

(defn part-one-main [& args]
  (println (reduce + (map hashf (split input-string))))
  )

;
; PART II
;

(defn init-boxes []
  (zipmap (range 256) (cycle [[]]))
  )

(defn to-box [box info]
  (if (nil? (:lens info))
    (loop [left box out []]
      (if (empty? left)
        out
        (recur (rest left) (if (= (first (first left)) (:string info)) out (conj out (first left))))
        )
      )
    (let [[match] (filter #(= (first %) (:string info)) box)]
      (if match
        (replace {match [(:string info) (:lens info)]} box)
        (conj box [(:string info) (:lens info)])
        ))
    )
  )

(defn step->info [step]
  (let [[string val] (clojure.string/split step #"(\=|\-)")]
    {:hash (hashf string)
     :string string
     :lens (if (nil? val) nil (Integer/parseInt val))
     }
    )
  )

(defn exec-sequence [info-list]
  (loop [left info-list boxes (init-boxes)]
    (if (empty? left)
      boxes
      (recur
        (rest left)
        (let [grab (first left) {h :hash} grab box (boxes h)]
          (assoc boxes h (to-box box grab))
          ))
      )
    )
  )

(defn box-val [box n]
  (reduce + (map-indexed (fn [i lens] (* (+ 1 n) (+ 1 i) (second lens))) box))
  )

(defn -main [& args]
  (println (reduce + (map (fn [[n box]] (box-val box n)) (exec-sequence (map step->info (split input-string))))))
  )