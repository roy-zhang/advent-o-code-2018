(ns day-3
  (:require [utils :as utils]))

(utils/get-str-input "input/3")

;; dumb way is a [][] containing a list of ids
;; another way is combo, and a function that compares two patches
;; loop big-map remaining-list overlaps

;; id inches-left inches-top width height
(defn patch-str->patch [patch-str]
  (map read-string (re-seq #"[0-9]+" patch-str)))

(defn points [[id x y width height]]
  (let [xx (+ x width -1)
        yy (+ y height -1)]
    [[x y] [xx y] [xx yy] [x yy]]))


(defn overlaps? [patch-a patch-b])
