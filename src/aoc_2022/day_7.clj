(ns aoc-2022.day-7
  (:require [aoc-2022.common :as com]
            [clojure.string :as s]))


(defn cd-dir [line state]
  (let [dir (second (re-matches #"\$ cd (\w+)" line))
        wd (:working-dir state)]
    (assoc state :working-dir (conj wd dir))))

(defn cd-back [state]
  (let [wd (:working-dir state)]
    (assoc state :working-dir (rest wd))))

(defn cd-root [state]
  (assoc state :working-dir '("root")))

(defn build-dir-updates [file-size wd dirs dir-updates]
  (if (empty? wd)
    dir-updates
    (let [dir (s/join "/" wd)
          current-size (get dirs dir 0)
          new-size (+ current-size file-size)
          updates (assoc dir-updates dir new-size)]
      (recur file-size (rest wd) dirs updates))))

(defn file [line state]
  (let [size (Integer/parseInt
               (second (re-matches #"(\d+) .*" line)))
        wd (:working-dir state)
        old-dirs (:dirs state)
        updated-dirs (build-dir-updates size wd old-dirs {})
        new-dirs (merge old-dirs updated-dirs)]
    (assoc state :dirs new-dirs)))

(defn process-line [line state]
  (cond
    ; cd-dir
    (re-matches #"\$ cd \w+" line) (cd-dir line state)
    ; cd-back
    (re-matches #"\$ cd .." line) (cd-back state)
    ; cd-root
    (re-matches #"\$ cd /" line) (cd-root state)
    ; ls
    (re-matches #"\$ ls" line) state
    ; file
    (re-matches #"\d+ .*" line) (file line state)
    ; dir
    (re-matches #"dir \w+" line) state
    ;; Shouldn't happen
    :else (throw (ex-info "Couldn't parse" {:line line}))))

(defn process-lines
  ([lines]
   (process-lines lines {:working-dir ()
                         :dirs {}}))
  ([lines state]
   (if (empty? lines)
     state
     (let [updated-state (process-line (first lines) state)]
       (recur (rest lines) updated-state)))))

(defn p1 [file-name]
  (->> file-name
       (com/read-input)
       (process-lines)
       (:dirs)
       (vals)
       (filter #(>= 100000 %))
       (reduce +)))


(defn p2 [file-name]
  (let [dirs (->> file-name
                  (com/read-input)
                  (process-lines)
                  (:dirs))
        sizes (vals dirs)
        total-space 70000000
        required-space 30000000
        used-space (get dirs "root")
        free-space (- total-space used-space)
        amount-to-delete (- required-space free-space)]
    (->> sizes
         (filter #(<= amount-to-delete %))
         (apply min))))