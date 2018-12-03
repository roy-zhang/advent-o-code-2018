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

(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
                        (fn step [v-seqs]
                          (let [increment
                                (fn [v-seqs]
                                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                                    (if (= i -1) nil
                                      (if-let [rst (next (v-seqs i))]
                                        (assoc v-seqs i rst)
                                        (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
                            (when v-seqs
                              (cons (map first v-seqs)
                                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))


(defn find-one-off-dup [input]
  (some one-off? (cartesian-product input input)))

(defn part-2 []
  (let [input (map name (utils/get-input "input/2"))]
    (find-one-off-dup input)))