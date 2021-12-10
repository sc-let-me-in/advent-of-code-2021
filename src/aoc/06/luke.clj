(ns aoc.06.luke
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn ingest [filename]
  (-> filename
      io/resource
      slurp
      (#(str/split % #","))
      ((partial map str/trim))
      frequencies))

(defn iterate-day [freqs]
  {"8" (freqs "0" 0)
   "7" (freqs "8" 0)
   "6" (+ (freqs "7" 0) (freqs "0" 0))
   "5" (freqs "6" 0)
   "4" (freqs "5" 0)
   "3" (freqs "4" 0)
   "2" (freqs "3" 0)
   "1" (freqs "2" 0)
   "0" (freqs "1" 0)})

(defn iterate-days [start n]
  (nth (iterate iterate-day start) n))

(defn total-freqs [freqs]
  (reduce + (for [[_ freq] freqs] freq)))

(-> (ingest "aoc/06/luke.txt")
    (#(iterate-days % 80))
    total-freqs)

(-> (ingest "aoc/06/luke.txt")
    (#(iterate-days % 256))
    total-freqs)