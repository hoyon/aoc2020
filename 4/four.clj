(ns four
  (:require [clojure.string :as str]
            [clojure.walk :refer [keywordize-keys]]
            [clojure.spec.alpha :as s]))

(defn process-passport [line]
  (let [pairs (str/split line #"\s")
        vals (map #(str/split % #":") pairs)
        string-keys (into {} vals)]
    (keywordize-keys string-keys)))

(defn read-file []
  (as-> (slurp "input") v
        (str/split v #"\n\n")
        (map process-passport v)))

(s/def :a/passport (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                           :opt-un [::cid]))

(defn part1 [passports]
  (let [valid (filter #(s/valid? :a/passport %) passports)]
    (count valid)))

(defn validate-num [min max num]
  (let [n (Integer. num)]
    (and
     (>= n min)
     (<= n max))))

(defn validate-year [min max year]
  (and (re-matches #"\d{4}" year)
       (validate-num min max year)))

(s/def :b/byr (partial validate-year 1920 2002))
(s/def :b/iyr (partial validate-year 2010 2020))
(s/def :b/eyr (partial validate-year 2020 2030))

(s/def :b/hgt
  (fn [hgt]
    (let [[_ n unit] (re-matches #"(\d*)(cm|in)" hgt)]
      (if (and n unit)
        (case unit
          "cm" (validate-num 150 193 n)
          "in" (validate-num 59 76 n)
          :else false)))))

(s/def :b/hcl (partial re-matches #"#[a-f0-9]{6}"))
(s/def :b/ecl (partial contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}))
(s/def :b/pid (partial re-matches #"\d{9}"))

(s/def :b/passport (s/keys :req-un [:b/byr :b/iyr :b/eyr :b/hgt :b/hcl :b/ecl :b/pid]
                           :opt-un [:b/cid]))

(defn part2 [passports]
  (let [valid (filter #(s/valid? :b/passport %) passports)]
    (count valid)))

(defn -main []
  (let [passports (read-file)]
    (println (part1 passports))
    (println (part2 passports))))

(-main)
