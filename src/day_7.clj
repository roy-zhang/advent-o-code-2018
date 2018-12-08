(ns day-7
  (:require [utils]
            [clojure.set :as set]))

(defn get-input []
  (map (comp vec str) (utils/get-input "input/7")))

(defn unlocks [input]
  (reduce (fn [acc [step unlock-step]]
            (if (contains? acc step)
              (update acc step conj unlock-step)
              (assoc acc step [unlock-step])))
          {} input))

(defn requisites [input]
  (reduce (fn [acc [required-step step]]
            (if (contains? acc step)
              (update acc step conj required-step)
              (assoc acc step (set [required-step]))))
          {} input))

(defn available-now [requisites-map taken-set]
  (map first
    (filter (fn [[k v]]
              (set/subset? v taken-set))
            requisites-map)))

(defn available [unlocks]
  (set/difference (set (keys unlocks))
                  (set (flatten (vals unlocks)))))

(defn top-alphabetical [available]
  (first (apply sorted-set available)))

(defn part-1 []
  (let [input (get-input)]
    (apply str
      (loop [available (available (unlocks input))
             step->reqs (requisites input)
             return-me []]
        (if (empty? available)
          return-me
          (let [top (top-alphabetical available)
                return-me (conj return-me top)
                newly-available (available-now step->reqs (set return-me))]
            (recur (set (-> available
                            (disj top)
                            (set/union (set/difference (set newly-available)
                                                       (set return-me)))))
                   (dissoc step->reqs top)
                   return-me)))))))