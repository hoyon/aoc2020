(ns five
  (:require [clojure.string :as str]))

(defn process-line [line]
  {:column (subs line 0 7)
   :row (subs line 7)})

(defn read-file []
  (->> (slurp "input")
       (str/split-lines)
       (map process-line)))

(defn find-pos [min max a b s]
  (let [diff (- max min)
        mid (+ min (quot diff 2))]
    (if (empty? s)
      min
      (condp = (first s)
        a (find-pos min mid a b (subs s 1))
        b (find-pos (+ mid 1) max a b (subs s 1))))))

(defn seat-id [pass]
  (let [row (find-pos 0 127 \F \B (:column pass))
        col (find-pos 0 7   \L \R (:row    pass))]
    (+ (* row 8) col)))

(defn seat-ids [passes]
  (->> passes
       (map seat-id)
       (sort)))

(defn part1 [passes]
  (last (seat-ids passes)))

(defn part2 [passes]
  (let [seats (seat-ids passes)
        max (last seats)
        candidates (for [i (range 0 max) :when (not (.contains seats i))] i)]
    (last candidates)))

(defn -main []
  (let [passes (read-file)]
    (println (part1 passes))
    (println (part2 passes))))

(-main)
