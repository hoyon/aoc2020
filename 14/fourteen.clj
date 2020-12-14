(ns fourteen
  (:require [clojure.string :as str])
  (:import java.util.BitSet))

(defn char->bit [char i]
  (case char
    \X nil
    \1 {:val 1 :idx i}
    \0 {:val 0 :idx i}))

(defn parse-mask [line]
  (let [mask (subs line 7)
        bits (filter some? (map char->bit (reverse mask) (range 0 (count mask))))]
    {:op :mask
     :mask bits}))

(defn parse-mem [line]
  (let [[_ addr-str val-str] (re-matches #"mem\[(\d*)\] = (\d*)" line)]
    {:op :mem
     :addr (Long. addr-str)
     :val (Long. val-str)}))

(defn process-line [line]
  (if (str/starts-with? line "mask")
    (parse-mask line)
    (parse-mem line)))

(defn read-file []
  (->> (slurp "input")
       (str/split-lines)
       (map process-line)))

(defn solve1 [input]
  (reduce
   (fn [[acc mask] cmd]
     (case (:op cmd)
       :mask [acc (:mask cmd)]
       :mem (let [new (reduce
                       (fn [acc {:keys [val idx]}]
                         (case val
                           0 (bit-clear acc idx)
                           1 (bit-set acc idx)))
                       (:val cmd)
                       mask)]
              [(assoc acc (:addr cmd) new) mask])))
   [{} []]
   input))

(defn part1 [input]
  (let [[mem _](solve1 input)]
    (apply + (vals mem))))

(defn -main []
  (let [input (read-file)]
    (println (part1 input))))

(-main)
