(ns aoc.04.nick
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [aoc.03.nick :refer [transpose]]))

(defn parse-nums [nums]
  (map read-string nums))

(defn parse-file [name]
  (let [[nums & boards]
        (-> name io/resource slurp (#(str/split % #"\n\n")))]
    [(parse-nums (str/split nums #","))
     (->> boards
          ((partial map str/split-lines))
          ((partial map (partial map #(str/split (str/trim %) #"\s+"))))
          ((partial map (partial map parse-nums))))]))

(def example (parse-file "aoc/04/example.txt"))
(def input (parse-file "aoc/04/nick.txt"))

(defn mark [num board]
  (map (partial map #(if (= num %) 'x %)) board))

(defn any-equal? [[l & ls]]
  (if l (or (apply = l) (recur ls)) false))

(defn bingo? [board]
  (any-equal? (concat board (transpose board))))

(defn sum-row [row]
  (reduce #(if (number? %2) (+ %1 %2) %1) 0 row))

(defn sum-unmarked [board]
  (reduce + (map sum-row board)))

(defn calc-score [[num board]]
  (* (sum-unmarked board) num))

;; Part 1
(defn first-to-win [[nums boards]]
  (loop [[n & ns] nums
         boards boards]
    (let [marked (map (partial mark n) boards)]
      (if-some [winner (first (filter bingo? marked))]
        [n winner]
        (recur ns marked)))))

(comment (-> example first-to-win calc-score))
(-> input first-to-win calc-score)

;; Part 2
(defn last-to-win [[nums boards]]
  (loop [[n & ns] nums
         boards boards]
    (let [marked (map (partial mark n) boards)
          remaining (filter (comp not bingo?) marked)]
      (if (empty? remaining)
        [n (first marked)]
        (recur ns remaining)))))

(comment (-> example last-to-win calc-score))
(-> input last-to-win calc-score)

