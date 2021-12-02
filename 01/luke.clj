(ns shit.city.advent.luke
  (:require
    [clojure.java.io :as io]))


;; parseInt can't be used in a map because it's from java so it needs to be wrapped
(defn intParse [s] (Integer/parseInt s))

;; read given filename in
(defn readFile [filename]
  (map intParse (with-open [rdr (io/reader filename)](doall (line-seq rdr)))))

;; calculate increases from list of numbers
(defn increases [nums idx prev]
  (cond
    (or (not (coll? nums)) (empty? nums)) 0
    (== idx 0) (increases (rest nums) (+ idx 1) (first nums))
    (> (first nums) prev) (+ 1 (increases (rest nums) (+ idx 1) (first nums)))
    :else
    (increases (rest nums) (+ idx 1) (first nums))
  ))

(println (increases (readFile (first *command-line-args*)) 0 0))