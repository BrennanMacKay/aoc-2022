(ns aoc-2022.day-5
  (:require [aoc-2022.common :as com]
            [clojure.string :as string]))

(defn value-at [line, column]
  (let [start (+ 1 (* column 4))
        end (+ start 1)
        v (subs line start end)]
    (if (= "." v)
      nil
      v)))

(defn read-row
  ([line, columns, state]
   (read-row line columns state 0))
  ([line, columns, state, cur-col]
   (if (>= cur-col columns)
     state
     (let [stack (get state cur-col)
           v (value-at line cur-col)
           updated (if (nil? v)
                     stack
                     (conj stack v))
           next-state (assoc state cur-col updated)
           next-col (+ cur-col 1)]
       (recur line columns next-state next-col)))))

(defn read-rows
  ([lines columns]
   (read-rows lines columns {}))
  ([lines, columns state]
   (if (empty? lines)
     state
     (let [next-state (read-row (first lines) columns state)]
       (recur (rest lines) columns next-state)))))

(defn read-diag [file-name columns]
  (let [file-name (str "diag-" file-name)
        lines (reverse (com/read-input file-name))
        init-state (read-rows lines columns)]
    init-state))

(defn read-step [line]
  (let [pattern #"move (\d+) from (\d+) to (\d+)"
        keys '(:count :from :to)
        values (map #(Integer/parseInt %)
                    (rest (re-matches pattern line)))]
    (zipmap keys values)))

(defn read-steps [file-name]
  (->> file-name
       (str "steps-")
       (com/read-input)
       (map read-step)))

(defn take-step
  ([state step crane-func]
   (let [from (- (:from step) 1)
         to (- (:to step) 1)
         move (take (:count step) (get state from))
         new-source (drop (:count step) (get state from))
         new-dest (concat (crane-func move) (get state to))
         new-state (assoc state from new-source to new-dest)]
     new-state)))

(defn run-steps [state steps crane-func]
  (if (empty? steps)
    state
    (recur (take-step state (first steps) crane-func)
           (rest steps) crane-func)))

(defn p1 [file-name columns]
  (let [init-state (read-diag file-name columns)
        steps (read-steps file-name)
        final-state (into (sorted-map) (run-steps init-state steps reverse))
        firsts (map #(first (second %)) final-state)]
    (clojure.pprint/pprint final-state)
    (println firsts)
    (string/join firsts)))

(defn p2 [file-name columns]
  (let [init-state (read-diag file-name columns)
        steps (read-steps file-name)
        final-state (into (sorted-map) (run-steps init-state steps identity))
        firsts (map #(first (second %)) final-state)]
    (string/join firsts)))
