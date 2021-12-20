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

(defn count-uniques [jlines]
  (reduce + (map (fn [l] (:output l)) jlines)))

(ingest "luke.txt")