(ns thirteen
  (:require [clojure.string :as str]))

(defn parse-bus-ids [id-string]
  (as-> id-string v
    (str/split v #",")
    (map #(if (= "x" %) % (Integer. %)) v)))

(defn read-file []
  (let [[earliest bus-ids] (str/split-lines (slurp "input"))]
    {:earliest (Integer. earliest)
     :bus-ids (parse-bus-ids bus-ids)}))

(defn part1 [{:keys [bus-ids earliest]}]
  (let [ids (filter integer? bus-ids)
        mods (map #(vector % (- % (mod earliest %))) ids)
        first-depart (first (sort-by second mods))]
    (apply * first-depart)))

(defn part2 [{:keys [bus-ids]}]
  (let [zipped (map vector (range 0 (count bus-ids)) bus-ids)
        buses (filter #(integer? (second %)) (rest zipped))]
    (first
     (reduce
      (fn [[acc increment] [offset id]]
        (loop [n acc]
          (if (= 0 (mod (+ n offset) id))
            [n (* increment id)]
            (recur (+ n increment))))
        )
      [0 (first bus-ids)]
      buses))))

(defn -main []
  (let [input (read-file)]
    (println (part1 input))
    (println (part2 input))))

(-main)
