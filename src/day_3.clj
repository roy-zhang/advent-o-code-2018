(ns day-3
  (:require [utils :as utils]))

(utils/get-str-input "input/3")

;; id inches-left inches-top width height
(defn str->patch [patch-str]
  (map read-string (re-seq #"[0-9]+" patch-str)))

(defn points [[id x y width height]]
  (let [xx (+ x width -1)
        yy (+ y height -1)]
    [id [x y] [xx yy]]))

(defn all-points [[id [tlx tly][brx bry]]]
  (utils/cartesian-product (range tlx (inc brx)) (range tly (inc bry))))

(defn points-within [[id [tlx tly] [brx bry]] points]
  (let [in? (fn [[px py]]
              (and (>= px tlx) (<= px brx)
                   (>= py tly) (<= py bry)))]
    (filter in? (all-points points))))

(defn part-1 []
  (let [input (->> (utils/get-str-input "input/3")
                   (map (comp points str->patch)))
        combos (->> (utils/cartesian-product input input)
                    (remove #(>= (ffirst %) (first (second %)))))]
    (count
      (reduce (fn [overlapers [points-1 points-2]]
                (apply (partial conj overlapers) (points-within points-1 points-2)))
              #{} combos))))

(defn overlaps? [points-a points-b]
  (not (empty? (points-within points-a points-b))))

(defn find-unique-patch [input]
  (let [combos (->> (utils/cartesian-product input input)
                    (remove #(>= (ffirst %) (first (second %)))))]
    (reduce (fn [unoverlapped [points-1 points-2]]
              (if (overlaps? points-1 points-2)
                (disj unoverlapped points-1 points-2)
                unoverlapped))
            (set input) combos)))

(defn part-2 []
  (->> (utils/get-str-input "input/3")
       (map (comp points str->patch))
       find-unique-patch))