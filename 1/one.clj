(ns one
  (:require [clojure.string :as str]))

(def target 2020)

(defn read-file []
  (->> (slurp "input")
       (str/split-lines)
       (map #(Integer. %))
       (sort)))

(defn part1 [small big]
  (let [[answer] (for [s small
                       b big 
                       :when (= target (+ s b))]
                   (* s b))]
    answer))

(defn part2 [small big]
  (let [[answer] (for [s1 small
                       s2 small
                       b big 
                       :when (= target (+ s1 s2 b))]
                   (* s1 s2 b))]
    answer))

(defn -main []
  (let [numbers (read-file)
        [small big] (partition-by #(< % (/ target 2)) numbers)]
    (println (part1 small big))
    (println (part2 small big))))
