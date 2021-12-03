(ns aoc.01.w
  (:require [clojure.string]))

(def input (clojure.string/split-lines (slurp *in*)))
(def input-nums (map (fn [n-string] (Integer/parseInt n-string)) input))

(defn sonar [nums]
  (second (reduce (fn [acc value]
            (let [prev-val (first acc)
                  sum (second acc)]
              (list value (+ sum (if (< prev-val value) 1 0)))))
          (list Integer/MAX_VALUE 0)
          nums)))

(defn convert-to-single [l]
  (loop [items l
         acc '()]
    (cond
      (< (count items) 3) (reverse acc)
      :else (recur
              (rest items)
              (cons (reduce + (take 3 items)) acc)))))

(def one (sonar input-nums))
(def two (sonar (convert-to-single input-nums)))

(println "answers:")
(println one)
(println two)
