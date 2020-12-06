(ns six
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-file []
  (-> (slurp "input")
      (str/split #"\n\n")))

(defn process-form-1 [formdata]
  (let [letters (str/replace formdata #"\s" "")]
    (distinct letters)))

(defn part1 [forms]
  (->> (read-file)
       (map process-form-1)
       (map count)
       (reduce + 0)))

(defn process-form-2 [formdata]
  (let [forms (str/split formdata #"\n")
        sets (map #(into #{} %) forms)]
    (apply set/intersection sets)))

(defn part2 [forms]
  (->> forms
       (map process-form-2)
       (map count)
       (reduce + 0)))

(defn -main []
  (let [forms (read-file)]
    (println (part1 forms))
    (println (part2 forms))))

(-main)
