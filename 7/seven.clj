(ns seven
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-bag [bag-str]
  (let [[_ n bag] (re-matches #"(\d*)\s(.*)" bag-str)]
                {:count (if (some? n) (Integer. n) nil)
                 :bag bag}))

(defn process-rule-str [rule-str]
  (let [rules (str/split rule-str #"[,.]")]
    (->> rules
         (map #(-> %
                   (str/replace #"bags?" "")
                   (str/trim)
                   (parse-bag)))
         (filter #(not= {:count nil :bag nil} %)))))

(defn process-line [line]
  (let [[container rule-str] (str/split line #" bags contain ")
        rules (process-rule-str rule-str)]
    {:container container
     :contents (into [] rules)}))

(defn read-file []
  (->> (slurp "input")
       (str/split-lines)
       (map process-line)))

(defn rule-contains-bag [target rule]
  (some #(= (:bag %) target) (:contents rule)))

(defn container-for [bags target]
  (->> bags
       (filter (partial rule-contains-bag target))
       (map :container)))

(defn part1-solver [bags target]
  (loop [targets [target] acc #{}]
    (let [containers (map (partial container-for bags) targets)
          bag-set (set (flatten containers))]
      (if (empty? (set/difference bag-set acc))
        acc
        (recur (into [] (set/difference bag-set acc)) (set/union bag-set acc))))))

(defn inner-bag-count [bags current]
  (let [[bag] (filter #(= (:container %) current) bags)
        counts (map #(* (:count %) (inc (inner-bag-count bags (:bag %)))) (:contents bag))]
    (apply + counts)))

(defn part1 [bags]
  (count (part1-solver bags "shiny gold")))

(defn part2 [bags]
  (inner-bag-count bags "shiny gold"))

(defn -main []
  (let [bags (read-file)]
    (println (part1 bags))
    (println (part2 bags))))

(-main)
