(ns aoc.01.luke
  (:require
    [clojure.java.io :as io]))


(def parse-int 
  "Wrapper for parse int that allows it to be mapped"
  (fn [s] (Integer/parseInt s)))

;; read given filename in
(defn readFile [filename]
  (map parse-int (with-open [rdr (io/reader filename)](doall (line-seq rdr)))))

;; calculate increases from list of numbers (part 1)
(defn increases [nums idx prev]
  (cond
    (or (not (coll? nums)) (empty? nums)) 0
    (== idx 0) (increases (rest nums) (+ idx 1) (first nums))
    (> (first nums) prev) (+ 1 (increases (rest nums) (+ idx 1) (first nums)))
    :else
    (increases (rest nums) (+ idx 1) (first nums))
  ))

;; number of sliding window increases (part 2)
(defn slidingWindowIncreases [nums]
  (let [newNum (nth nums 3 nil)]
    (cond
      (nil? newNum) 0
      (> newNum (first nums)) (+ 1 (slidingWindowIncreases (rest nums)))
      :else
      (slidingWindowIncreases (rest nums)))))

;; print solutions
(comment
  (let [nums (readFile (first *command-line-args*))]
    (println (increases nums 0 0))

    (println (slidingWindowIncreases nums))))

