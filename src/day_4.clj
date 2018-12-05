(ns day-4
  (:require [utils]
            [java-time :as t]
            [clojure.string :as str]))

(defn parse-date [date-str]
  (t/local-date-time "yyyy-MM-dd HH:mm" date-str))

(defn minutes-between [d1 d2]
  (t/time-between d1 d2 :minutes))

(defn durations [actions]
  (map #(apply minutes-between %)
       (partition 2 1 (map :date actions))))

(defn with-durations [actions]
  (map (fn [action minutes]
         (assoc action :duration minutes))
       actions (durations actions)))

(defn actions [filename]
  (->> (utils/get-str-input filename)
    (map (fn [line]
           (let [split (str/split line #"\s")]
             {:date (parse-date (str (first split) " " (second split)))
              :info (let [x (read-string (last split))]
                      (if (= clojure.lang.Symbol (type x))
                        (name x) x))})))
    (sort-by :date)
    with-durations))

(defn max-of [dict]
  (apply max-key val dict))

(defn guard+most-minutes-asleep [actions]
  (let [guard->asleep-time (dissoc
                             (reduce (fn [guard->asleep {:keys [info duration]}]
                                       (if (int? info)
                                         (assoc guard->asleep :guard-on-duty info)
                                         (let [on-duty (:guard-on-duty guard->asleep)]
                                           (if (= "asleep" info)
                                             (utils/accum guard->asleep on-duty duration)
                                             guard->asleep))))
                                     {} actions)
                             :guard-on-duty)]
    [(first (max-of guard->asleep-time)) (apply max (vals guard->asleep-time))]))

(defn actions-of-guard [actions guard]
  (let [on-guard (atom false)]
    (filter (fn [action]
              (if (int? (:info action))
                (reset! on-guard (= guard (:info action)))
                @on-guard))
            actions)))

(defn fill-in-accum [accum keys]
  (reduce (fn [acc key]
            (utils/accum acc key 1))
          accum keys))

(defn minute-most-asleep+days-asleep-on [guard-actions]
  (let [asleep-actions (filter #(= "asleep" (:info %)) guard-actions)]
    (if (empty? asleep-actions)
      [0 0]
      (let [minute->times-asleep (reduce (fn [minute->times-asleep {:keys [date duration]}]
                                           (fill-in-accum minute->times-asleep (take duration (drop (.getMinute date) (range)))))
                                         {} asleep-actions)]
        (max-of minute->times-asleep)))))

(defn part-1 []
  (let [input (actions "input/4")
        [guard minutes] (guard+most-minutes-asleep input)
        [minute days] (minute-most-asleep+days-asleep-on (actions-of-guard input guard))]
    (* guard minute)))

(defn all-guards [actions]
  (->> (filter #(int? (:info %)) actions)
       (map :info)
       (into #{})))

(defn part-2 []
  (let [input (actions "input/4")
        guard-ids (disj (all-guards input) 2441 3433 241)
        guard->min+days-asleep (into {} (map #(vec [% (minute-most-asleep+days-asleep-on (actions-of-guard input %))])
                                             guard-ids))
        [guard [minute days]] (apply max-key (comp last val) guard->min+days-asleep)]
    (* guard minute)))