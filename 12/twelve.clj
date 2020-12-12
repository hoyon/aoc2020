(ns twelve
  (:require [clojure.string :as str]))

(defn process-line [line]
  (let [op (subs line 0 1)
        n (Integer. (subs line 1))]
    {:op op :n n}))

(defn read-file []
  (->> (slurp "input")
       (str/split-lines)
       (map process-line)))

(defn dir->int [dir]
  (case dir
    :n 0
    :e 1
    :s 2
    :w 3))

(defn int->dir [i]
  (let [normalised (mod i 4)]
    (case normalised
      0 :n
      1 :e
      2 :s
      3 :w)))

(defn rotate [dir degrees]
  (let [turns (/ degrees 90)
        new (+ (dir->int dir) turns)]
    (int->dir new)))

(defn move-forward [ferry cmd]
  (case (:dir ferry)
    :n (update ferry :y #(+ % (:n cmd)))
    :s (update ferry :y #(- % (:n cmd)))
    :e (update ferry :x #(+ % (:n cmd)))
    :w (update ferry :x #(- % (:n cmd)))))

(defn solve1 [input]
  (reduce
   (fn [acc cmd]
     (case (:op cmd)
       "N" (update acc :y #(+ % (:n cmd)))
       "S" (update acc :y #(- % (:n cmd)))
       "E" (update acc :x #(+ % (:n cmd)))
       "W" (update acc :x #(- % (:n cmd)))
       "L" (update acc :dir #(rotate % (- (:n cmd))))
       "R" (update acc :dir #(rotate % (:n cmd)))
       "F" (move-forward acc cmd)))
   {:dir :e :x 0 :y 0}
   input))

(defn part1 [input]
  (let [final (solve1 input)]
    (+ (Math/abs (:x final)) (Math/abs (:y final)))))

(defn sin [deg]
  (Math/round (Math/sin (Math/toRadians deg))))

(defn cos [deg]
  (Math/round (Math/cos (Math/toRadians deg))))

(defn rotate-waypoint [{:keys [way-x way-y] :as state} cw]
  (let [acw (- cw)
        x (- (* way-x (cos acw)) (* way-y (sin acw)))
        y (+ (* way-x (sin acw)) (* way-y (cos acw)))]
    (assoc state :way-x x :way-y y)))

(defn move-towards-waypoint [{:keys [way-x way-y ferry-x ferry-y] :as state} times]
  (let [move-x (* way-x times)
        move-y (* way-y times)]
    (assoc state
           :ferry-x (+ ferry-x move-x)
           :ferry-y (+ ferry-y move-y))))

(defn solve2 [input]
  (reduce
   (fn [acc cmd]
     (case (:op cmd)
       "N" (update acc :way-y #(+ % (:n cmd)))
       "S" (update acc :way-y #(- % (:n cmd)))
       "E" (update acc :way-x #(+ % (:n cmd)))
       "W" (update acc :way-x #(- % (:n cmd)))
       "L" (rotate-waypoint acc (- (:n cmd)))
       "R" (rotate-waypoint acc (:n cmd))
       "F" (move-towards-waypoint acc (:n cmd))))
   {:way-x 10 :way-y 1 :ferry-x 0 :ferry-y 0}
   input))

(defn part2 [input]
  (let [final (solve2 input)]
    (+ (Math/abs (:ferry-x final)) (Math/abs (:ferry-y final)))))

(defn -main []
  (let [input (read-file)]
    (println (part1 input))
    (println (part2 input))))

(-main)
