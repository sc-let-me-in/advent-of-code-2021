(ns aoc.02.nick
  (:require [clojure.string :as str]))

(def example '((forward 5)
               (down 5)
               (forward 8)
               (up 3)
               (down 8)
               (forward 2)))
(def input
  (-> "02/input/nick.txt"
      slurp
      str/split-lines
      ((partial map #(map read-string (str/split % #" "))))))

(defn f1 [[instr x] [d p _]]
  (condp = instr
    'forward [d (+ p x) _]
    'up      [(- d x) p _]
    'down    [(+ d x) p _]))

(defn f2 [[instr x] [d p a]]
  (condp = instr
    'forward [(+ d (* a x)) (+ p x) a]
    'up      [d p (- a x)]
    'down    [d p (+ a x)]))

(defn calc-course [cmds f]
  (loop [[cmd & cmds] cmds
         [d p a] [0 0 0]]
    (if cmd
      (recur cmds (f cmd [d p a]))
      (* d p))))

;; Part 1
(comment (calc-course example f1))
(calc-course input f1)

;; Part 2
(comment (calc-course example f2))
(calc-course input f2)
