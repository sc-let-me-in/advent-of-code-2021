(ns aoc.05.nick
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-line [line]
  (map read-string
       (next (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line))))

(defn parse [file]
  (-> file
      io/resource
      slurp
      str/split-lines
      ((partial map parse-line))))

(def example (parse "aoc/05/example.txt"))
(def input (parse "aoc/05/nick.txt"))

(defn make-points [diags? [x1 y1 x2 y2]]
  (cond (= x1 x2)
        (map vector
             (range (min y1 y2) (inc (max y1 y2)))
             (repeat (inc (Math/abs (- y1 y2))) x1))
        (= y1 y2)
        (map vector
             (repeat (inc (Math/abs (- x1 x2))) y1)
             (range (min x1 x2) (inc (max x1 x2))))
        (not diags?) nil
        :else
        (let [slope  (/ (- y1 y2) (- x1 x2))
              b (- y1 (* slope x1))]
          (map #(vector % (+ (* slope %) b))
               (range (min x1 x2) (inc (max x1 x2)))))))

(defn count-overlaps [nums diags?]
  (->> nums
       (map (partial make-points diags?))
       (mapcat identity)
       frequencies
       (filter #(> (second %) 1))
       count))

(comment
  (count-overlaps example false)
  (count-overlaps example true))

(count-overlaps input false)
(count-overlaps input true)
