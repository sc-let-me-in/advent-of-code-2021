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

;; Maybe the solution is the median? (this worked for pt 1???)
(defn pseudo-median [list]
  (let [sorted (sort list)]
    (nth sorted (quot (count sorted) 2))))

(defn fuel-calc [list posn]
  (reduce + (map (fn [n] (Math/abs (- n posn))) list)))

(defn actual-fuel-calc [list posn]
  (reduce + (map (fn [n]
                   (let [d (Math/abs (- n posn))]
                     (reduce + (range (+ 1 d))))) list)))

;; awful, exponential time garbage
(let [l (ingest "luke.txt") lmax (reduce max l)]
  (reduce min (map (partial actual-fuel-calc l) (range lmax))))