(ns day-2
  (:require [utils :as utils]))

(defn c->count [id]
  (reduce (fn [m i]
            (if (contains? m i)
              (update m i inc)
              (assoc m i 1))) {} id))

(defn has-2? [id]
  (contains? (set (vals (c->count id))) 2))

(defn has-3? [id]
  (contains? (set (vals (c->count id))) 3))

(defn part-1 []
  (let [input (map name (utils/get-input "input/2"))]
    (* (count (filter has-2? input))
       (count (filter has-3? input)))))


(defn one-off? [[str-a str-b]]
  (let [has? (->> (map = str-a str-b)
                 (remove true?)
                 count
                 (= 1))]
    (if has?
      [str-a str-b]
      false)))


(defn find-one-off-dup [input]
  (some one-off? (utils/cartesian-product input input)))

(defn part-2 []
  (let [input (map name (utils/get-input "input/2"))]
    (find-one-off-dup input)))