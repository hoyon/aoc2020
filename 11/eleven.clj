(ns eleven
  (:require [clojure.string :as str]))

(defn str->cell [s]
  (case s
    \. :floor
    \L :empty))

(defn process-row [line]
  (vec (map str->cell line)))

(defn read-file []
  (->> (slurp "input")
       (str/split-lines)
       (map process-row)
       (vec)))

(defn create-state []
  (let [grid (read-file)]
    {:grid grid
     :size-x (count (first grid))
     :size-y (count grid)
     :changed false}))

(defn get-cell [state x y]
  (if (or (>= x (:size-x state)) (>= y (:size-y state)) (< x 0) (< y 0))
    nil
    (get-in state [:grid y x])))

(defn adjacent-seats [state x y]
  (let [n  (get-cell state x (dec y))
        ne (get-cell state (inc x) (dec y))
        e  (get-cell state (inc x) y)
        se (get-cell state (inc x) (inc y))
        s  (get-cell state x (inc y))
        sw (get-cell state (dec x) (inc y))
        w  (get-cell state (dec x) y)
        nw (get-cell state (dec x) (dec y))
        combined [n ne e se s sw w nw]]
    (count (filter #(= :occupied %) combined))))

(defn put-cell [state x y status]
  (-> state
      (assoc-in [:grid y x] status)
      (assoc :changed true)))

(def coords
  (memoize (fn [size-x size-y] (for [x (range size-x) y (range size-y)] [x y]))))

(defn step [initial-state config]
  (reduce
   (fn [state [x y]]
     (let [cell (get-cell initial-state x y)
           c (delay ((:caster config) initial-state x y))]
       (cond
         (and (= cell :empty) (= 0 @c)) (put-cell state x y :occupied)
         (and (= cell :occupied) (>= @c (:threshold config))) (put-cell state x y :empty)
         :else state))
     )
   (assoc initial-state :changed false)
   (coords (:size-x initial-state) (:size-y initial-state))))

(defn simulate [initial-state step-config]
  (loop [state initial-state]
    (let [updated (step state step-config)]
      (if-not (:changed updated)
        updated
        (recur updated)))))

(defn part1 [state]
  (let [simulated (simulate state {:caster adjacent-seats :threshold 4})]
    (->> (:grid simulated)
         (flatten)
         (filter #(= :occupied %))
         (count))))

(defn cast-ray [state x y offset-x offset-y]
  (loop [x x y y]
    (let [new-x (+ x offset-x)
          new-y (+ y offset-y)
          cell (get-cell state new-x new-y)]
      (case cell
        nil nil
        :floor (recur new-x new-y)
        cell))))

(defn cast-rays [state x y]
  (let [n  (cast-ray state x y  0 -1)
        ne (cast-ray state x y  1 -1)
        e  (cast-ray state x y  1  0)
        se (cast-ray state x y  1  1)
        s  (cast-ray state x y  0  1)
        sw (cast-ray state x y -1  1)
        w  (cast-ray state x y -1  0)
        nw (cast-ray state x y -1 -1)
        combined [n ne e se s sw w nw]]
    (count (filter #(= :occupied %) combined))))

(defn part2 [state]
  (let [simulated (simulate state {:caster cast-rays :threshold 5})]
    (->> (:grid simulated)
         (flatten)
         (filter #(= :occupied %))
         (count))))

(defn -main []
  (let [state (create-state)]
    (println (part1 state))
    (println (part2 state))))

(-main)
