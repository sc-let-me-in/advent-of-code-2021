(ns aoc.07.luke
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc.01.luke :as luke]))

(defn comma-split [str]
  (str/split str #","))

(defn ingest [filename]
  (->> (str "aoc/07/" filename)
       io/resource
       slurp
       comma-split
       (map luke/parse-int)))

;; Maybe the solution is the median?
(defn pseudo-median [list]
  (let [sorted (sort list)]
    (nth sorted (quot (count sorted) 2))))

(defn fuel-calc [list posn]
  (reduce + (map (fn [n] (Math/abs (- n posn))) list)))

(let [l (ingest "luke.txt")]
  (fuel-calc l (pseudo-median l)))