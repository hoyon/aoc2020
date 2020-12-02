(ns two
  (:require [clojure.string :as str]))

(defn read-file []
  (->> (slurp "input")
       (str/split-lines)
       (map process-line)))

(defn letter->char [letter]
  (let [bytes-array (.getBytes letter)]
    (char (first bytes-array))))

(defn process-line [line]
  (let [[min max letter _ password] (str/split line #"[- :]")]
    {:min (Integer. min)
     :max (Integer. max)
     :letter (letter->char letter)
     :password password}))

(defn valid-1? [{:keys [min max letter password]}]
  (let [matches (filter #(= letter %) password)
        cnt (count matches)]
    (and (>= cnt min) (<= cnt max))))

(defn part1 [input]
  (let [valid (filter valid-1? input)]
    (count valid)))

(defn valid-2? [{:keys [min max letter password]}]
  (let [c1 (nth password (- min 1))
        c2 (nth password (- max 1))]
    (or
     (and (= c1 letter) (not= c2 letter))
     (and (= c2 letter) (not= c1 letter)))))

(defn part2 [input]
  (let [valid (filter valid-2? input)]
    (count valid)))

(defn -main []
  (let [input (read-file)]
    (println (part1 input))
    (println (part2 input))))

(-main)
