(ns shit.city.advent.luke
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn mapOverFileLineSeq [func filename]
  (map func (with-open [rdr (io/reader filename)] (doall (line-seq rdr)))))

(defn getFinalPos [commands currX currZ]
  (if (empty? commands) [currX currZ]
      (let [currCmd (first commands) restCmd (rest commands)]
        (cond
         (= "forward" (first currCmd)) (getFinalPos restCmd (+ currX (Integer/parseInt (second currCmd))) currZ)
         (= "up" (first currCmd)) (getFinalPos restCmd currX (- currZ (Integer/parseInt (second currCmd))))
         (= "down" (first currCmd)) (getFinalPos restCmd currX (+ currZ (Integer/parseInt (second currCmd))))))))

(let [finalPos
      (getFinalPos
       (mapOverFileLineSeq
        (fn [str] (str/split str #" ")) 
        "/home/nluken/Documents/advent-of-code-2021/02/input.txt")
       0 0)]

(print (* (first finalPos) (second finalPos))))