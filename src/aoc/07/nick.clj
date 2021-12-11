(ns aoc.07.nick
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def example '(16 1 2 0 4 2 7 1 2 14))
(def input (->> "aoc/07/nick.txt"
                io/resource
                slurp
                (#(str/split % #","))
                (map read-string)))

(defn p1-cost [pos] #(Math/abs (- pos %)))
(defn tri [n] (/ (* n (inc n)) 2))
(defn p2-cost [pos] (comp tri (p1-cost pos)))

(defn calc-cost [nums f]
  (reduce + (map f nums)))

(defn calc-lowest-cost [nums f]
  (->> nums
       (apply max) inc range
       (map #(calc-cost nums (f %)))
       (apply min)))

(comment (calc-lowest-cost example p1-cost)
         (calc-lowest-cost example p2-cost))

(calc-lowest-cost input p1-cost)
(calc-lowest-cost input p2-cost)
