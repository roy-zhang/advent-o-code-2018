(ns day-5
  (:require [utils]))

(defn opposites? [p]
  (when (= 2 (count p))
    (let [[a b] p]
      (and (not= a b)
           (= (clojure.string/capitalize a)
              (clojure.string/capitalize b))))))

(defn one-shrink [input]
  (let [pairs        (partition-all 2 1 input)
        delete-next? (atom false)]
    (map first
         (remove (fn [pair]
                   (if @delete-next?
                     (do (reset! delete-next? false)
                         true)
                     (if (opposites? pair)
                       (do (reset! delete-next? true)
                           true))))
                 pairs))))

;;um might want to look into not needing  -Xss1024m for increasing stack size
(defn collapse-and-count [input]
  (count (loop [input input]
           (let [new-input (one-shrink input)]
             (if (= input new-input)
               new-input
               (recur new-input))))))

(defn part-1 []
  (let [input (first (utils/get-str-input "input/5"))]
    (collapse-and-count input)))

(defn part-2 []
  (let [input    (first (utils/get-str-input "input/5"))
        test-mes (map (fn [letter]
                        (clojure.string/replace input
                                                (re-pattern (str "[" letter (clojure.string/capitalize letter) "]")) ""))
                      "r")
        counts   (pmap collapse-and-count test-mes)]
    (min counts)))