(ns shit.city.advent.luke
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

;; make this public- it could be useful
(def map-file-line-seq (fn [func filename]
                              (map func (with-open [rdr (io/reader filename)] (doall (line-seq rdr))))))

(defn calc-simple-final-pos [commands x z]
  (if (empty? commands) [x z]
      (let [cmd (first commands) rst (rest commands)]
        (cond
          (= "forward" (first cmd)) (calc-simple-final-pos rst (+ x (second cmd)) z)
          (= "up" (first cmd)) (calc-simple-final-pos rst x (- z (second cmd)))
          (= "down" (first cmd)) (calc-simple-final-pos rst x (+ z (second cmd)))))))

(defn calc-aimed-final-pos [commands x z pitch]
  (if (empty? commands) [x z]
      (let [cmd (first commands) rst (rest commands)]
        (cond
          (= "forward" (first cmd)) (calc-aimed-final-pos rst
                                                          (+ x (second cmd))
                                                          (+ z (* (second cmd) pitch)) pitch)
          (= "up" (first cmd)) (calc-aimed-final-pos rst x z
                                                     (- pitch (second cmd)))
          (= "down" (first cmd)) (calc-aimed-final-pos rst x z
                                                       (+ pitch (second cmd)))))))

(let [commands (map-file-line-seq
                (fn [str]
                  (let [strCmd (str/split str #" ")]
                    [(first strCmd) (Integer/parseInt (second strCmd))]))
                "/home/nluken/Documents/advent-of-code-2021/02/input.txt")
      simple-final-pos (calc-simple-final-pos commands 0 0)
      aimed-final-pos (calc-aimed-final-pos commands 0 0 0)]

  ;; answers
  (print (* (first simple-final-pos) (second simple-final-pos)))
  (print (* (first aimed-final-pos) (second aimed-final-pos))))