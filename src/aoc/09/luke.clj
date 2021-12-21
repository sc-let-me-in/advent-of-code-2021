(ns aoc.09.luke
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc.01.luke :as luke]))

(defn nat-pair? [p]
  "Are both numbers in the pair positive?"
  (and (nat-int? (first p)) (nat-int? (second p))))

(defn out-of-bounds? [p xlim ylim]
  (or (> (first p) xlim) (> (second p) ylim)))

(defn valid-coord? [p xlim ylim]
  (and (nat-pair? p) (not (out-of-bounds? p xlim ylim))))

(defn ingest [filename]
  (->> filename
       (str "aoc/09/")
       io/resource
       slurp
       str/split-lines
       (map (fn [s] (str/split s #"")))
       (map (fn [l] (map luke/parse-int l)))))

(defn grid-get [grid x y]
  (-> grid
      (nth y)
      (nth x)))

(defn get-surrounding-nums [grid x y xlim ylim]
  (let [coords [[(+ x 1) y] [(- x 1) y] [x (+ y 1)] [x (- y 1)]]
        valid-coords (filter (fn [c] (valid-coord? c xlim ylim)) coords)]
    (map (fn [coord] (grid-get grid (first coord) (second coord))) valid-coords))

(defn get-low-point-nums [grid]
  (let [xlim (dec (count (first grid))) ylim (count (rest grid))]
    (loop [y 0
           remgrid grid
           lpnums '()]
      (if (empty? remgrid) lpnums
          (recur (inc y) (rest remgrid)
                 (concat lpnums
                  (loop [row (first remgrid) x 0 lpts '()]
                    (cond (empty? row) (reverse lpts)
                          (< (first row)
                             (reduce min (get-surrounding-nums grid x y xlim ylim))) (recur (rest row)
                                                                                            (inc x)
                                                                                            (cons (first row) lpts))
                          :else (recur (rest row) (inc x) lpts)))))))))

(let [input (ingest "luke.txt")]
  (get-low-point-nums input))
