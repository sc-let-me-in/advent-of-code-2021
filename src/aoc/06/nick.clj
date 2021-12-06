(ns aoc.06.nick
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def example '(3 4 3 1 2))
(def input (-> "aoc/06/nick.txt"
               io/resource slurp
               (str/split #",")
               ((partial map read-string))))

;; Part 1 - lol @ me for thinking an AoC problem mentioning
;;          exponential growth won't bite me in the ass for part 2
(defn update-fish [lf]
  (if (zero? lf) '(6 8) (list (dec lf))))

(defn update-all [fish]
  (reduce #(concat (update-fish %2) %1) '() fish))

(defn run-laternfish [fish days]
  (if (zero? days)
    fish
    (recur (update-all fish) (dec days))))

(comment (count (run-laternfish example 80)))
(count (run-laternfish input 80))

;; Part 2
(defn create-counts [fish]
  (reduce #(update %1 %2 inc) (vec (repeat 9 0)) fish))

(defn simulate-day [counts]
  (update (vec (concat (rest counts) [(first counts)]))
          6
          #(+ (first counts) %)))

(defn simulate-days [counts days]
  (if (zero? days)
    counts
    (recur (simulate-day counts) (dec days))))

(defn lanternfish-count [fish days]
  (apply + (simulate-days (create-counts fish) days)))

(comment
  (lanternfish-count example 80)
  (lanternfish-count input 80)
  (lanternfish-count example 256))
(lanternfish-count input 256)
