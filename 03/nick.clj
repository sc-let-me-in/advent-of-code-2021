(ns aoc.03.nick
  (:require [clojure.string :as str]))

(defn split [nums] (mapv #(mapv read-string (str/split % #"")) nums))

(def example
  (split '("00100"
           "11110"
           "10110"
           "10111"
           "10101"
           "01111"
           "00111"
           "11100"
           "10000"
           "11001"
           "00010"
           "01010")))
(def input (-> "03/input/nick.txt" slurp str/split-lines split))

;; Part 1
(defn transpose
  "Flips a matrix"
  [m]
  (apply mapv vector m))

(defn bin-to-dec
  "Converts list of bits to decimal"
  [n]
  (Integer/parseInt (str/join n) 2))

(defn freq-count
  "Returns a pair of bits and thir count, ordered by most frequent"
  [ns]
  (sort-by val > (frequencies ns)))

(defn freq-pair
  "Returns a pair of the most and least frequent bit"
  [ns]
  (map key (freq-count ns)))

(defn calc-rates
  "Calculates the gamma and epsilon values for the given numbers"
  [ns]
  (map bin-to-dec (transpose (map freq-pair (transpose ns)))))

(comment (apply * (calc-rates example)))
(apply * (calc-rates input))

;; Part 2
(defn o2-gen [[[val c1] [_ c2]]]
  (if (= c1 c2) 1 val))
(defn co2-scrub [[[_ c1] [val c2]]]
  (if (= c1 c2) 0 val))

(defn col-freqs
  "Calculates the frequency of 0's and 1's at the given column index"
  [idx nums]
  (freq-count (mapv #(nth % idx) nums)))

(defn val-at-idx?
  "Does the given value exist at the index in the list?"
  [val idx l]
  (= (nth l idx) val))

(defn filter-crit
  "Returns numbers which satisfy the bit criteria at the given index"
  [det-bit-crit idx nums]
  (let [bit (det-bit-crit (col-freqs idx nums))]
    (filter (partial val-at-idx? bit idx) nums)))

(defn calc-rate
  "Calculates rating determined by the bit criteria function for the given numbers"
  [nums det-bit-crit]
  (loop [rem nums
         idx 0]
    (if (next rem)
      (recur (filter-crit det-bit-crit idx rem) (inc idx))
      (bin-to-dec (first rem)))))

(comment (* (calc-rate example o2-gen)
            (calc-rate example co2-scrub)))
(apply * (map (partial calc-rate input) [o2-gen co2-scrub]))
