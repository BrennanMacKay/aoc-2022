(ns aoc-2022.common
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn read-input [file-name]
  (-> file-name
      (io/resource)
      (slurp)
      (string/split-lines)))
