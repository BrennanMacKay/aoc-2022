(ns aoc-2022.day-10
  (:require [aoc-2022.common :as com]
            [clojure.string :as s]))

(defn init-state [inspect-act] {:reg         1
                                :cycle       1
                                :inspect-at  inspect-act
                                :inspections []})

(defn new-state [reg cycle inspect-at inspections]
  {:reg reg :cycle cycle :inspect-at inspect-at :inspections inspections})

(defn inspect [reg cycle inspect-at inspections]
  (if (contains? inspect-at cycle)
    (conj inspections [cycle reg])
    inspections))

(defn addx [{:keys [reg cycle inspect-at inspections]} v]
  (let [inspections (inspect reg cycle inspect-at inspections)
        inspections (inspect reg (inc cycle) inspect-at inspections)
        reg (+ reg v)
        cycle (+ 2 cycle)]
    (new-state reg cycle inspect-at inspections)))

(defn noop [{:keys [reg cycle inspect-at inspections]}]
  (let [inspections (inspect reg cycle inspect-at inspections)]
    (new-state reg (inc cycle) inspect-at inspections)))

(defn run-step [state [step v]]
  #_(println state)
  (case step
    "addx" (addx state v)
    "noop" (noop state)))

(defn run-steps
  [{:keys [reg cycle inspect-at inspections] :as state} [step & rem-steps]]
  (if (nil? step)
    (let [inspections (inspect reg cycle inspect-at inspections)]
      (new-state reg cycle inspect-at inspections))
    (let [next-state (run-step state step)]
      (recur next-state rem-steps))))

(defn read-steps [input]
  (->> input
       (map #(s/split % #" "))
       (map (fn [[step value]]
              [step (some-> value
                            (Integer/parseInt))]))))


(defn p1
  ([file-name]
   (p1 file-name #{20 60 100 140 180 220}))
  ([file-name inspect-at]
   (let [input (com/read-input file-name)
         steps (read-steps input)
         final-state (run-steps (init-state inspect-at) steps)]
     (clojure.pprint/pprint final-state)
     (->> final-state
          (:inspections)
          (map (partial reduce *))
          (reduce +)))))


(defn print-pixel [[cycle v]]
  (let [pixel (dec (mod cycle 40))]
    (if (>= 1 (abs (- pixel v)))
      (print "#")
      (print "."))))

(defn print-line [cycles]
  (doseq [c cycles]
    (print-pixel c))
  (print "\n"))

(defn print-lines [cycles]
  (doseq [c cycles]
    (print-line c)))

(defn p2
  ([file-name]
   (p2 file-name (set (range 0 241))))
  ([file-name inspect-at]
   (let [input (com/read-input file-name)
         steps (read-steps input)
         final-state (run-steps (init-state inspect-at) steps)]
     #_(clojure.pprint/pprint (:inspections final-state))
     (->> final-state
          (:inspections)
          (partition 40)
          (print-lines)))))

