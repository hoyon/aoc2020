(ns three
  (:require [clojure.string :as str]))

(defn process-line [line]
  (let [tiles (str/split line #"")]
    (map #(= "#" %) tiles)))

(defn read-file []
  (->> (slurp "input")
       (str/split-lines)
       (map process-line)))

(defn reducer [right [x tree-count] line]
  (let [line-length (count line)
        new-x (mod (+ right x) line-length)
        new-tree-count (if (nth line x)
                         (inc tree-count)
                         tree-count)]
    [new-x new-tree-count]))

(defn solve [right down lines]
  (let [[_ answer] (reduce (partial reducer right)
                           [0 0]
                           (take-nth down lines))]
    answer))

(defn part1 [lines]
  (solve 3 1 lines))

(defn part2 [lines]
  (*
   (solve 1 1 lines)
   (solve 3 1 lines)
   (solve 5 1 lines)
   (solve 7 1 lines)
   (solve 1 2 lines)))

(defn -main []
  (let [input (read-file)]
    (println (part1 input))
    (println (part2 input))))

(-main)
