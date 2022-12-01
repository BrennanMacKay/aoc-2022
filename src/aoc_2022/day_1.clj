(ns aoc-2022.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn read-input [file-name]
  (-> file-name
      (io/resource)
      (slurp)
      (string/split-lines)))

(defn read-elfs
  ([input]
   (read-elfs input {:food ()} ()))
  ([input current-elf all-elfs]
   (if (empty? input)
     (conj all-elfs current-elf)
     (let [head (first input)
           rest (rest input)]
       (if (empty? head)
         (recur rest {:food ()} (conj all-elfs current-elf))
         (recur rest
                (update-in current-elf [:food] conj
                           (Integer/parseInt head))
                all-elfs))))))

(defn summed-food [elfs]
  (map #(reduce + (:food %)) elfs))

(defn max-calories [calorie-counts]
  (reduce max calorie-counts))

(defn max-three-calories [calorie-counts]
  (->> calorie-counts
       (sort)
       (take-last 3)
       (reduce +)))

(defn p1 [file-name]
  (-> file-name
      (read-input)
      (read-elfs)
      (summed-food)
      (max-calories)))

(defn p2 [file-name]
  (-> file-name
      (read-input)
      (read-elfs)
      (summed-food)
      (max-three-calories)))