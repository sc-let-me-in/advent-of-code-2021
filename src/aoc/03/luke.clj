(ns aoc.03.luke 
  (:require [aoc.02.luke :as luke]))

(defn input []
  (luke/map-file-line-seq (fn [str] (map (fn [c] (- (int c) 48)) (seq str))) "resources/aoc/03/luke.txt"))

;; go through all rows. add 1 for 1, subtract 1 for 0. Positive = 1, negative = 0
(defn find-common-sequence [rows]
  (let [start-scores
        (loop [row (first rows)
               scores '()]
          (if (empty? row) (reverse scores) (recur (rest row) (cons 0 scores))))]

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
                     (if (= 1 (first row))
                       (recur (rest row) (rest old) (cons (+ 1 (first old)) updated))
                       (recur (rest row) (rest old) (cons (- (first old) 1) updated))))))))))

(def exp (fn [n pow]
           (loop [n n pow pow tot 1]
             (if (= 0 pow) tot
                 (recur n (- pow 1) (* n tot))))))

;; convert binary to dec
(defn convert-to-dec [binary-seq]
  (loop [remaining (reverse binary-seq) tally 0 power 0]
    (if (empty? remaining) tally
        (recur (rest remaining) (+ tally (* (exp 2 power) (first remaining))) (+ 1 power)))))

(let [final-seq (find-common-sequence (input))]
  (print (* (convert-to-dec final-seq) (convert-to-dec (map (fn [x] (- 1 x)) final-seq)))))