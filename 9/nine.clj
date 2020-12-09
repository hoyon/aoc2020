(ns nine
  (:require [clojure.string :as str]))

(defn read-file []
  (->> (slurp "input")
       (str/split-lines)
       (map #(BigInteger. %))))

(defn is-sum? [nums target]
  (let [sums (for [x nums y nums :when (= target (+ x y))] (+ x y))]
    (seq sums)))

(defn reducer-1 [input acc n]
  (let [nums (subvec input (- acc 25) acc)
        target (nth input acc)]
    (if (is-sum? nums target)
      (inc acc)
      (reduced target))))

(defn part1 [input]
  (reduce (partial reducer-1 input) 25 input))

(defn get-sum [input target start]
  (reduce
   (fn [acc n]
     (let [new-sum (+ acc (nth input n))]
       (condp apply [new-sum target]
         = (reduced (inc n))
         < new-sum
         > (reduced nil))))
   0
   (range start (count input))))

(defn part2-indices [input target]
  (->> (range (count input))
       (map #(vector % (get-sum input target %)))
       (filter #(and (some? (nth % 1))
                     (< 1 (- (nth % 1)
                             (nth % 0)))))
       (first)))

(defn part2 [input target]
  (let [[a b] (part2-indices input target)
        nums (subvec input a b)]
    (+ (apply max nums) (apply min nums))))

(defn -main []
  (let [input (vec (read-file))
        answer1 (part1 input)]
    (println answer1)
    (println (str (part2 input answer1)))))

(-main)
