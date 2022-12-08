(ns aoc-2022.day-8
  (:require [aoc-2022.common :as com]
            [clojure.string :as s]))

;; Direction is "from" with 0, 0 upper left corner
(def starting-tree-vis {:north true
                        :south true
                        :east  true
                        :west  true})

(defn starting-visibility [width]
  (vec (repeat (* width width) starting-tree-vis)))

(defn in-bounds? [width coord]
  (let [x (:x coord)
        y (:y coord)]
    (and (< x width)
         (>= x 0)
         (< y width)
         (>= y 0))))

(defn index-at [width coord]
  (+ (:x coord) (* width (:y coord))))

(defn coord-at [width idx]
  {:x (mod idx width) :y (quot idx width)})

(defn get-at [grid width coord]
  (nth grid (index-at width coord)))

(defn set-at [grid width coord value]
  (assoc grid (index-at width coord) value))

(defn next-coord [dir coord]
  (case dir
    :north (update coord :y inc)
    :south (update coord :y dec)
    :east (update coord :x dec)
    :west (update coord :x inc)))

;; handle an individual slice
(defn from-dir-slice [dir grid width coord max-height visibility]
  (if (not (in-bounds? width coord))
    visibility
    (let [cur-height (get-at grid width coord)
          cur-tree-vis (get-at visibility width coord)
          visible? (< max-height cur-height)
          new-tree-vis (assoc cur-tree-vis dir visible?)
          new-vis (set-at visibility width coord new-tree-vis)
          new-max (max cur-height max-height)
          next-coord (next-coord dir coord)]
      #_(println dir coord max-height cur-height visible?)
      (recur dir grid width next-coord new-max new-vis))))

;; next row/column
(defn next-start [dir coord]
  (case dir
    :north (update coord :x inc)
    :south (update coord :x inc)
    :east (update coord :y inc)
    :west (update coord :y inc)))

;; handle all the slices
(defn from-dir [dir grid width starting-coord visibility]
  (if (not (in-bounds? width starting-coord))
    visibility
    (let [new-vis (from-dir-slice dir grid width starting-coord -1 visibility)
          next (next-start dir starting-coord)]
      (recur dir grid width next new-vis))))

(defn to-ints [input]
  (->> input
       (char-array)
       (vec)
       (mapv int)
       (mapv #(- % 48))))

(defn read-input-p1 [input]
  (let [width (count (first input))
        max-coord (- width 1)
        grid (->> input
                  (s/join)
                  (to-ints))]
    (->> (starting-visibility width)
         (from-dir :north grid width {:x 0, :y 0})
         (from-dir :south grid width {:x 0, :y max-coord})
         (from-dir :east grid width {:x max-coord, :y 0})
         (from-dir :west grid width {:x 0, :y 0}))))

(defn p1 [file-name]
  (->> file-name
       (com/read-input)
       (read-input-p1)
       (map vals)
       (map (partial some identity))
       (filter identity)
       (count)))


(defn vis-from-dir [dir grid width start-height coord dist]
  (cond
    (not (in-bounds? width coord)) dist
    (>= (get-at grid width coord) start-height) (inc dist)
    :else (recur dir grid width start-height (next-coord dir coord) (inc dist))))

(defn calc-view-score [grid width idx height]
  (let [coord (coord-at width idx)
        dist-n (vis-from-dir :north grid width height (next-coord :north coord) 0)
        dist-s (vis-from-dir :south grid width height (next-coord :south coord) 0)
        dist-e (vis-from-dir :east grid width height (next-coord :east coord) 0)
        dist-w (vis-from-dir :west grid width height (next-coord :west coord) 0)]
    (* dist-n dist-s dist-e dist-w)))



(defn read-input-p2 [input]
  (let [width (count (first input))
        grid (->> input
                  (s/join)
                  (to-ints))]
    (map-indexed #(calc-view-score grid width %1 %2) grid)))


(defn p2 [file-name]
  (->> file-name
       (com/read-input)
       (read-input-p2)
       (reduce max)))