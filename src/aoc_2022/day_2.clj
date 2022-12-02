(ns aoc-2022.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(declare scissors)
(declare paper)
(declare rock)

(def rock {:v "A"
           :beats "C"
           :loses "B"
           :score 1})
(def paper {:v "B"
            :beats "A"
            :loses "C"
            :score 2})
(def scissors {:v "C"
               :beats "B"
               :loses "A"
               :score 3})

(def types-p1 {"A" rock
               "X" rock
               "B" paper
               "Y" paper
               "C" scissors
               "Z" scissors})

(defn to-types-p1 [pair]
  [(types-p1 (first pair))
   (types-p1 (second pair))])

(defn type-for-result [other result]
  (cond
    (= result "X") (types-p1 (:beats other))
    (= result "Y") other
    (= result "Z") (types-p1 (:loses other))))

(defn to-types-p2 [pair]
  (let [first (types-p1 (first pair))
        second (type-for-result first (second pair))]
    [first second]))

(defn read-input [file-name]
  (-> file-name
      (io/resource)
      (slurp)
      (string/split-lines)))

(defn to-round [to-type-fn input]
  (-> input
        (string/split #" ")
        (to-type-fn)))

(defn to-rounds [input to-type-fn]
  (map (partial to-round to-type-fn) input))

(defn win? [round]
  (= (:beats (second round))
     (:v (first round))))

(defn draw? [round]
  (= (:v (second round))
     (:v (first round))))

(defn win-score [round]
  (cond
    (win? round) 6
    (draw? round) 3
    :else 0))

(defn round-score [round]
  (+ (win-score round)
     (:score (second round))))

(defn rounds-score [rounds]
  (->> rounds
       (map round-score)
       (reduce +)))

(defn p1 [file-name to-type-fn]
  (-> file-name
      (read-input)
      (to-rounds to-type-fn)
      (rounds-score)))
