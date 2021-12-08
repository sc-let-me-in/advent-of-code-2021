(ns aoc.04.luke
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [aoc.01.luke :as luke]))


(defrecord BingoGame [callouts boards])
(defrecord BingoSquare [digit called?])
(defrecord VictoryInfo [board unmarked-sum last-num])

(defn decode-board [bstr]
  (map (fn [sv] (map (fn [s] (BingoSquare. (luke/parse-int s) false))
                     (filter not-empty (str/split sv #" "))))
       (str/split-lines bstr)))

(defn decode-split-str [bv]
  (BingoGame. (map luke/parse-int (str/split (first bv) #"\,"))
              (mapv decode-board (rest bv))))

(defn ingest [filename] (-> filename
                            io/resource
                            slurp
                            ((fn [s] (str/split s #"\n\n")))
                            decode-split-str))

(defn transpose [board]
  (loop [pos 0 new '()]
    (if (== (count (first board)) pos) new
        (recur (inc pos) (cons (flatten (map (fn [r] (nth r pos)) board)) new)))))

(comment
  ;test the transpose function
  (def ex-matrix [[1, 2], [3, 4], [5, 6]])
  (transpose ex-matrix))

(defn check-row [r] (every? :called? r))

(defn check-for-victory [board]
  (or (some check-row board)
      (some check-row (transpose board))))

;; didn't actually need this
(defn get-winning-nums [board]
  (map :digit (flatten (filter check-row (concat board (transpose board))))))

(defn get-unmarked-nums [board]
  (map :digit (flatten (map (fn [r] (filter (fn [s] (not (:called? s))) r)) board))))

(defn check-all-for-victory [boards]
  (let [board-vics (map check-for-victory boards)]
    (loop [pos 0]
      (cond
        (== pos (count boards)) -1
        (nth board-vics pos) pos
        :else (recur (inc pos))))))

(defn mark-board [board callout]
  (map (fn [r] (map (fn [sq]
                      (BingoSquare. (:digit sq)
                                    (or (:called? sq) (== (:digit sq) callout)))) r)) board))

(defn winning-game-loop [game]
  (loop [state game last -1]
    (let [victory-pos (check-all-for-victory (:boards state))]
      (if-not (> victory-pos 0)
        (recur (BingoGame. (rest (:callouts state))
                           (map (fn [b] (mark-board b (first (:callouts state))))
                                (:boards state)))
               (first (:callouts state)))
        (VictoryInfo. victory-pos
                      (reduce + (get-unmarked-nums (nth (:boards state) victory-pos)))
                      last)))))
(comment (winning-game-loop (ingest "aoc/04/example.txt"))
         (winning-game-loop (ingest "aoc/04/luke.txt")))

(println "final answer:"
         (let [winning-state (winning-game-loop (ingest "aoc/04/luke.txt"))]
           (* (:unmarked-sum winning-state) (:last-num winning-state))))

;; part 2- losing
(defn losing-game-loop [game]
  (loop [state game last -1]
    (if-not (= 1 (count (:boards state)))
      (recur (BingoGame. (rest (:callouts state))
                         (remove check-for-victory
                                 (map (fn [b] (mark-board b (first (:callouts state)))) (:boards state))))
             (first (:callouts state)))
      (* (reduce + (get-unmarked-nums (first (:boards state)))) last))))

(comment 
  (losing-game-loop (ingest "aoc/04/example.txt"))
  )
(println "part 2: " (losing-game-loop (ingest "aoc/04/luke.txt")))