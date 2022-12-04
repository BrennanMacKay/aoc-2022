(ns aoc-2022.day-4
  (:require [aoc-2022.common :as com]
            [clojure.string :as string]))


(defn read-area [area]
  (as-> area v
      (string/split v #"-")
      (map #(Integer/parseInt %) v)))

(defn split-pairs [pairs]
  (->> pairs
       (map #(string/split % #","))
       (map #(map read-area %))))

(defn within? [this-area, other-area]
  (and (>= (first this-area) (first other-area))
       (<= (second this-area) (second other-area))))

(defn point-within? [point, area]
  (and (>= point (first area))
       (<= point (second area))))

(defn overlap? [this-area, other-area]
  (or (point-within? (first this-area) other-area)
      (point-within? (second this-area) other-area)))

(defn bad-pair? [f pair]
  (or (f (first pair) (second pair))
      (f (second pair) (first pair))))


(defn p1 [file-name]
  (->> file-name
       (com/read-input)
       (split-pairs)
       (map (partial bad-pair? within?))
       (filter identity)
       (count)))

(defn p2 [file-name]
  (->> file-name
       (com/read-input)
       (split-pairs)
       (map (partial bad-pair? overlap?))
       (filter identity)
       (count)))
