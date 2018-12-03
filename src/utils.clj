(ns utils)

(defn get-str-input [filename]
  (->> (slurp filename)
       clojure.string/split-lines))

(defn get-input [filename]
  (map read-string (get-str-input filename)))

