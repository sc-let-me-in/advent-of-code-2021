(ns aoc.03.luke 
  (:require [aoc.02.luke :as luke]))

(defn input []
  (luke/map-file-line-seq (fn [str] (map (fn [c] (- (int c) 48)) (seq str))) "resources/aoc/03/luke.txt"))

;; go through all rows. add 1 for 1, subtract 1 for 0. Positive = 1, negative = 0
(defn find-common-sequence [rows]
  (let [start-scores (repeat (count (first rows)) 0)]
    (loop [remaining rows
           scores start-scores]
      (if (empty? remaining)
        (loop [scores scores
               final-nums '()]
          (if (empty? scores) (reverse final-nums)
              (if (>= (first scores) 0)
                (recur (rest scores) (cons 1 final-nums))
                (recur (rest scores) (cons 0 final-nums)))))
        (recur (rest remaining)
               (loop [row (first remaining) old scores updated '()]
                 (if (empty? row) (reverse updated)
                     (recur (rest row) 
                            (rest old) 
                            (cons (+ (- (* 2 (first row)) 1) (first old)) 
                                  updated)))))))))

(defn bit-elimination [rows keep-fn]
  (loop [remaining rows
         pos 0]
    (if (> (count remaining) 1)
      (recur (let [most-common (find-common-sequence remaining)]
               (loop [a remaining kept '()]
                 (cond (empty? a) kept
                       (keep-fn (first a) most-common pos) (recur (rest a) (cons (first a) kept))
                       :else (recur (rest a) kept))))
             (inc pos))
      (first remaining))))

(defn get-oxygen-reading [rows]
  (bit-elimination rows (fn [row most-common pos]
                          (= (nth most-common pos) (nth row pos)))))

(defn get-scrubber-reading [rows]
  (bit-elimination rows (fn [row most-common pos]
                          (not (= (nth most-common pos) (nth row pos))))))

;; might be useful to have this
(def exp (fn [n pow]
           (loop [n n pow pow tot 1]
             (if (= 0 pow) tot
                 (recur n (- pow 1) (* n tot))))))

;; convert binary to dec
(defn convert-to-dec [binary-seq]
  (loop [remaining (reverse binary-seq) tally 0 power 0]
    (if (empty? remaining) tally
        (recur (rest remaining) (+ tally (* (exp 2 power) (first remaining))) (+ 1 power)))))

(let [rows (input)
      final-seq (find-common-sequence rows)
      oxy-reading (get-oxygen-reading rows)
      scrubber-reading (get-scrubber-reading rows)]
  (println "Power Consumption Reading: "
           (* (convert-to-dec final-seq) (convert-to-dec (map (fn [x] (- 1 x)) final-seq))))
  (println "Oxygen Reading:" (convert-to-dec oxy-reading))
  (println "CO2 Scrubber Reading:" (convert-to-dec scrubber-reading)))
