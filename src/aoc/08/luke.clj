(ns aoc.08.luke 
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defrecord JournalLine [signals output])

(defn pipe-split [s]
  (map str/trim (str/split s #"\|")))

(defn parse-line [l]
  (let [strings (pipe-split l)]
    (JournalLine. (str/split (first strings) #" ") 
                  (str/split (second strings) #" "))))

(defn ingest [filename]
  (->> (str "aoc/08/" filename)
       io/resource
       slurp
       str/split-lines
       (map parse-line)))

;; accept 2, 3, 4, or 7
(defn unique-num [n]
  (or (= n 2) (= n 3) (= n 4) (= n 7)))

(defn count-uniques [jlines]
  (->> jlines
       (map (fn [l] (map count (:output l))))
       (map (fn [l] (filter unique-num l)))
       (map count)
       (reduce +)))

(let [input (ingest "luke.txt")]
  (count-uniques input))