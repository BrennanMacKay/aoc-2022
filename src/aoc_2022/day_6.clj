(ns aoc-2022.day-6
  (:require [aoc-2022.common :as com]
            [clojure.string :as string]))

(defn find-first-distinct
  ([line look-for]
   (find-first-distinct line look-for look-for
                             (count line)))
  ([line look-for index size]
   (if (> look-for size)
     index
     (let [cur (take look-for line)]
       (if (apply distinct? cur)
         index
         (recur (rest line)
                look-for
                (inc index)
                (dec size)))))))

(defn p1 [file-name]
  (-> file-name
      (com/read-input)
      (first)
      (seq)
      (find-first-distinct 4)))

(defn p2 [file-name]
  (-> file-name
      (com/read-input)
      (first)
      (seq)
      (find-first-distinct 14)))
