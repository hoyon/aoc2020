(ns eight
  (:require [clojure.string :as str]))

(defn process-line [line]
  (let [[ins arg] (str/split line #" ")]
    {:op ins
     :arg (Integer. arg)}))

(defn read-file []
  (->> (slurp "input")
       (str/split-lines)
       (map process-line)))

(defn run-program [instructions]
  (let [acc (atom 0)
        ran (atom [])]
    (loop [pc 0]
      (if (or (.contains @ran pc) (>= pc (count instructions)))
        [@acc pc]
        (let [ins (nth instructions pc)]
          (swap! ran #(cons pc %))
          (recur (+ pc (case (:op ins)
                         "acc" (do (swap! acc + (:arg ins)) 1)
                         "jmp" (:arg ins)
                         "nop" 1))))))))

(defn part1 [instructions]
  (nth (run-program instructions) 0))

(defn gen-variants [instructions]
  (reduce
   (fn [[acc n] ins]
     (case (:op ins)
       "acc" [acc (inc n)]
       "jmp" [(cons (assoc instructions n (assoc ins :op "nop")) acc) (inc n)]
       "nop" [(cons (assoc instructions n (assoc ins :op "jmp")) acc) (inc n)]))
   [[] 0]
   instructions))

(defn part2 [instructions]
  (let [ins-count (count instructions)
        [variants _] (gen-variants (vec instructions))
        results (map run-program variants)
        [answer] (filter #(>= (nth % 1) ins-count) results)]
    (nth answer 0)))

(defn -main []
  (let [input (read-file)]
    (println (part1 input))
    (println (part2 input))))

(-main)
