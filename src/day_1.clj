(ns day-1
  (:require [utils :as utils]))

;; part 1

(defn part-1 []
  (reduce + (utils/get-input "input/1")))

;; part 2
(defn first-freq-dup [num-list]
  (loop [seen-set #{}
         num-list (cycle num-list)
         freq 0]
    (if (contains? seen-set freq)
        freq
        (recur (conj seen-set freq) (rest num-list) (+ freq (first num-list))))))

(defn part-2 []
  (first-freq-dup (utils/get-input "input/1")))