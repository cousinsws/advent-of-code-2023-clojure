(ns adventofcode.dayseventeen)

(def sample-string "2413432311323\n3215453535623\n3255245654254\n3446585845452\n4546657867536\n1438598798454\n4457876987766\n3637877979653\n4654967986887\n4564679986453\n1224686865563\n2546548887735\n4322674655533")
(def input-string (slurp "input/17.txt"))

(defn grid [in-raw]
  (map (fn [line] (map #(- (int %) (int \0)) line)) (clojure.string/split-lines in-raw))
  )