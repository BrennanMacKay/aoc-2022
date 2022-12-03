(ns aoc-2022.day-3
  (:require [aoc-2022.common :as com]))

(defn split [sack]
  (let [halfway (/ (count sack) 2)]
    (split-at halfway sack)))

(defn item-to-score [item]
  (let [raw-score (int item)]
    (cond
      (< 96 raw-score) (- raw-score 96)
      :else (- raw-score 38))))

(defn comp-as-score [comp]
  (map item-to-score comp))

(defn sack-as-score [split-sac]
  (map comp-as-score split-sac))

(defn find-num-packed [num split-sac]
  (->> split-sac
       (map distinct)
       (flatten)
       (frequencies)
       (filter #(= num (val %)))
       (keys)))

(defn p1 [file-name]
  (->> file-name
       (com/read-input)
       (map split)
       (map sack-as-score)
       (map (partial find-num-packed 2))
       (flatten)
       (reduce +)))

(defn p2 [file-name]
  (->> file-name
       (com/read-input)
       (map #(seq %))
       (map comp-as-score)
       (partition 3)
       (map (partial find-num-packed 3))
       (flatten)
       (reduce +)))
