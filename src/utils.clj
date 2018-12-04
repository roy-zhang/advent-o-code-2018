(ns utils)

(defn get-str-input [filename]
  (->> (slurp filename)
       clojure.string/split-lines))

(defn get-input [filename]
  (map read-string (get-str-input filename)))

(defn accum [dict key number]
  (if (contains? dict key)
    (update dict key + number)
    (assoc dict key number)))

(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step (fn step [v-seqs]
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