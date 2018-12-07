(ns day-6
  (:require [utils]))

(defn get-input []
  (->>
    (utils/get-str-input "input/6")
    (map #(mapv read-string (clojure.string/split % #",\s")))))

(defn bounds [input]
  (let [by-x (group-by first input)
        big-x (first (apply max-key first input))
        small-x (first (apply min-key first input))
        by-y (group-by second input)
        big-y (second (apply max-key second input))
        small-y (second (apply min-key second input))]
    {:outermost (distinct
                  (concat (by-x small-x) (by-x big-x) (by-y small-y) (by-y big-y)))
     :bounds [[small-x small-y] [big-x big-y]]}))

(defn distance [[x y] [xx yy]]
  (+ (Math/abs (- y yy))
    (Math/abs (- x xx))))

(defn nearest-point [points [x y]]
  (let [by-distance (group-by (partial distance [x y]) points)
        nearest-distance (apply min (keys by-distance))
        nearest (by-distance nearest-distance)]
    (when (= 1 (count nearest))
      (first nearest))))

(defn largest-voronoi-count [input]
  (let [{:keys [outermost bounds]} (bounds input)
        [[small-x small-y] [big-x big-y]] bounds
        grid (utils/cartesian-product (range small-x (inc big-x))
                                      (range small-y (inc big-y)))
        voronoi (group-by (partial nearest-point input) grid)
        without-outermost (apply dissoc voronoi (conj outermost nil))]
     (apply max (map count (vals without-outermost)))))

;;3682 too high  [[252 74] 3682]
;;2917  [[140 211] 2917]

(defn total-distance [points xy]
  (apply +
    (pmap (partial distance xy) points)))

(defn central-cluster-count [input total-distance-cap]
  (let [{:keys [outermost bounds]} (bounds input)
        [[small-x small-y] [big-x big-y]] bounds
        grid (utils/cartesian-product (range small-x (inc big-x))
                                      (range small-y (inc big-y)))
        total-distances (map (partial total-distance input) grid)]
    (count (remove #(<= total-distance-cap %) total-distances))))