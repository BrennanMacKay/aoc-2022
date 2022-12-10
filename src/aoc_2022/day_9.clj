(ns aoc-2022.day-9
  (:require [aoc-2022.common :as com]
            [clojure.string :as s]))

(defn init-state [length] {:head    [0 0]
                           :parts   (vec (repeat (dec length) [0 0]))
                           :visited #{[0 0]}})



(defn next-head-coord [dir [x y]]
  (case dir
    "U" [x (inc y)]
    "D" [x (dec y)]
    "R" [(inc x) y]
    "L" [(dec x) y]))

(defn move-on-axis [this prev]
  (if (< this prev)
    (inc this)
    (dec this)))

(defn move-on-diag [[this-x this-y] [prev-x prev-y]]
  (let [x (move-on-axis this-x prev-x)
        y (move-on-axis this-y prev-y)]
    [x y]))

(defn distance [[this-x this-y] [prev-x prev-y]]
  (max (abs (- this-x prev-x))
       (abs (- this-y prev-y))))

(defn next-part-coord [[this-x this-y :as this] [prev-x prev-y :as prev]]
  (let [dist (distance this prev)]
    (cond
      (>= 1 dist) this
      (= this-x prev-x) [this-x (move-on-axis this-y prev-y)]
      (= this-y prev-y) [(move-on-axis this-x prev-x) prev-y]
      :else (move-on-diag this prev))))

(defn move-tail [prev [this & rem] state]
  (if (nil? this)
    (update state :visited #(conj % prev))
    (let [move-to (next-part-coord this prev)
          updated-state (update state :parts #(conj % move-to))]
      (recur move-to rem updated-state))))

(defn move-head [dir {:keys [head parts visited]}]
  (let [new-head (next-head-coord dir head)
        state {:head new-head :parts [] :visited visited}]
    (move-tail new-head parts state)))

(defn parse-input [input]
  (->> input
       (map #(s/split % #" "))
       (map (fn [[dir dist]] (repeat (Integer/parseInt dist) dir)))
       (flatten)))


(defn run-steps
  ([length steps]
   (run-steps length steps (init-state length)))
  ([length [this-step & next-steps] state]
   (if (nil? this-step)
     state
     (let [next-state (move-head this-step state)]
       (recur length next-steps next-state)))))

(defn p1 [file-name]
  (->> file-name
       (com/read-input)
       (parse-input)
       (run-steps 2)
       (:visited)
       (count)))


(defn p2 [file-name]
  (->> file-name
       (com/read-input)
       (parse-input)
       (run-steps 10)
       (:visited)
       (count)))