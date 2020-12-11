(ns ten
  (:require [clojure.string :as str]))

(defn read-file []
  (->> (slurp "input")
       (str/split-lines)
       (map #(Integer. %))
       (sort)
       (vec)))

(defn part1 [numbers]
  (let [shifted (cons 0 numbers)
        nums (conj numbers (+ 3 (last numbers)))
        diffs (map (fn [a b] (- b a)) shifted nums)
        one-count (count (filter #(= 1 %) diffs))
        three-count (count (filter #(= 3 %) diffs))]
    (* one-count three-count)))

(defn possible-next [numbers num]
  (let [a (filter #(= % (+ 1 num)) numbers)
        b (filter #(= % (+ 2 num)) numbers)
        c (filter #(= % (+ 3 num)) numbers)]
    {:n num :next (concat a b c)}))

(defn get-suffix-combos [nexts]
  (reduce
   (fn [acc {:keys [n next]}]
     (let [finishers (map #(get acc %) next)]
       (assoc acc n (apply + finishers))))
   (assoc {} (:n (last nexts)) 1)
   (rest (reverse nexts))))

(defn part2 [numbers]
  (let [nexts (map (partial possible-next numbers) numbers)
        suffix-combos (get-suffix-combos nexts)]
    (get suffix-combos 0)))

(defn -main []
  (let [numbers (read-file)]
    (println (part1 numbers))
    (println (part2 (cons 0 numbers)))))

(-main)
