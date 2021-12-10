(ns aoc.05.luke 
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [aoc.01.luke :refer [parse-int]]))

(defn parse-line [s]
  (map (fn [s] (map parse-int (str/split s #","))) 
       (map str/trim (str/split s #"->"))))

(defn ingest [filename] (-> filename
                            io/resource
                            slurp
                            str/split-lines
                            ((fn [l] (map parse-line l)))))

(defn make-line-strings [line]
  (let [start (first line) finish (second line)
        x0 (first start) y0 (second start)
        x1 (first finish) y1 (second finish)]
    (cond
      (= x0 x1) (map str (repeat (str x0 " ")) (range (min y0 y1) (+ 1 (max y0 y1))))
      (= y0 y1)
      (map str (range (min x0 x1) (+ 1 (max x0 x1))) (repeat (str " " (second start))))
      :else "")))

(defn count-overlapping-points [lines]
  (-> (for [[point freq]
            (frequencies (flatten (map make-line-strings lines)))
            :when (> freq 1)] point)
      ((fn [l] (filter not-empty l)))
      distinct
      count))

(count-overlapping-points (ingest "aoc/05/example.txt"))
(count-overlapping-points (ingest "aoc/05/luke.txt"))

