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
     :size-y (count grid)}))

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
    {:empty (count (filter #(= :empty %) combined))
     :occupied (count (filter #(= :occupied %) combined))}))

(defn put-cell [state x y status]
  (assoc-in state [:grid y x] status))

(def coords
  (memoize (fn [size-x size-y] (for [x (range size-x) y (range size-y)] [x y]))))

(defn step-1 [initial-state]
  (reduce
   (fn [state [x y]]
     (let [cell (get-cell initial-state x y)
           adj (adjacent-seats initial-state x y)]
       (cond
         (and (= cell :empty) (= 0 (:occupied adj))) (put-cell state x y :occupied)
         (and (= cell :occupied) (>= (:occupied adj) 4)) (put-cell state x y :empty)
         :else state))
     )
   initial-state
   (coords (:size-x initial-state) (:size-y initial-state))))

(defn simulate [initial-state]
  (loop [state initial-state]
    (let [updated (step-1 state)]
      (if (= state updated)
        updated
        (recur updated)))))

(defn part1 [state]
  (let [simulated (simulate state)]
    (->> (:grid simulated)
         (flatten)
         (filter #(= :occupied %))
         (count))))

(defn -main []
  (let [state (create-state)]
    (println (part1 state))))

(-main)
