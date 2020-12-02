(ns aoc2020.1
  (:require [clojure.string :as s]))

(def desired-total 2020)

(defn load-data [path]
  (->> (slurp path)
       (s/split-lines)
       (map #(Integer/parseInt %))
       (sort)))


(defn binary-search [target coll]
  (loop [nums coll]
    (when-not (empty? nums)
      (let [midpoint (/ (count nums) 2)
            value (nth nums midpoint)]
        (condp apply [value target]
          = value
          < (recur (drop (Math/ceil midpoint) nums))
          > (recur (take (Math/floor midpoint) nums)))))))

(defn find-terms [expected-total coll]
  (loop [[first-term & rest] coll]
    (if (nil? first-term)
      []
      (let [remainder (- expected-total first-term)
            second-term (binary-search remainder rest)]
        (if (nil? second-term)
          (recur rest)
          [first-term second-term])))))


(defn process-2 []
  (->> (load-data "data/1.txt")
       (find-terms desired-total)
       (apply *)))

(process-2)

(defn process-3 []
  (let [data (load-data "data/1.txt")]
    (loop [[first & rest] data]
      (let [remainder (- desired-total first)
            terms (find-terms remainder rest)]
        (if (empty? terms)
          (recur rest)
          (apply * (concat [first] terms)))))))

(process-3)

