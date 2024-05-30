(ns adventofcode.fcopy)

(def file (clojure.string/split-lines (slurp "input/shortRand.txt")))
(defn rate [line id]
  (when (#(and (= id (subs line 42 44)) (number? %)) (read-string (subs line 77 82))) (read-string (subs line 77 82)))
  )

(defn mean [coll]
  (/ (reduce + coll) (count coll))
  )

(defn square [n] (* n n))

(defn sdev [a]
  (Math/sqrt (/
               (reduce + (map square (map - a (repeat (mean a)))))
               (- (count a) 1 ))))

(defn -main [& args]
  ;(println (rate (first file) "02"))
  (doall [(println "ELA")
  (println (str "AVG: " (mean (filter identity (map #(rate % "01") file)))))
  (println (str "SDEV: " (sdev (filter identity (map #(rate % "01") file)))))
  (println "MATH")
  (println (str "AVG: " (mean (filter identity (map #(rate % "02") file)))))
  (println (str "SDEV: " (sdev (filter identity (map #(rate % "02") file)))))])
  )