(ns utils)

(defn get-input [filename]
    (->> (slurp filename)
      clojure.string/split-lines
      (map read-string)))