(ns aoc.01.nick
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def example '(199 200 208 210 200 207 240 269 260 263))
(def input (-> "aoc/01/nick.txt"
               io/resource
               slurp
               str/split-lines
               (#(map read-string %))))

#_(defn count-increases
    "Counts the number of times a number is followed by a bigger number in a given list"
    [nums]
    (loop [nums nums
           acc 0]
      (if (<= (count nums) 1)
        acc
        (recur (rest nums) (if (< (first nums) (second nums)) (inc acc) acc)))))

(defn count-increases-window
  "Counts the number of times a n-size window's sum is bigger than the next in a given list"
  [window nums]
  (loop [nums nums
         acc 0]
    (if (<= (count nums) window)
      acc
      (let [prev (reduce + (take window nums))
            next (reduce + (take window (drop 1 nums)))]
        (recur (rest nums) (if (< prev next) (inc acc) acc))))))

(def count-increases (partial count-increases-window 1))

;; Part 1
(comment (count-increases '(1 2))
         (count-increases '(2 1))
         (count-increases example))
(count-increases input)

;; Part 2
(comment (count-increases-window 3 '(1 2 3 4))
         (count-increases-window 3 '(4 3 2 1))
         (count-increases-window 3 example))
(count-increases-window 3 input)

