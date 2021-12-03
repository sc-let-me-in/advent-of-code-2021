(require '(clojure.string))

(def input (clojure.string/split-lines (slurp *in*)))

(defn navigate [input]
  (loop [moves input
         depth 0
         position 0]
    (if (empty? moves) (* depth position)
      (let [split-cmd (clojure.string/split (first moves) #" ")
            action (first split-cmd)
            value (Integer/parseInt (second split-cmd))]
        (cond
          (= action "forward")  (recur (rest moves) depth (+ position value))
          (= action "up")       (recur (rest moves) (- depth value) position)
          (= action "down")     (recur (rest moves) (+ depth value) position))))))

(defn navigate-aim [input]
  (loop [moves input
         depth 0
         aim 0
         position 0]
    (if (empty? moves) (* depth position)
      (let [split-cmd (clojure.string/split (first moves) #" ")
            action (first split-cmd)
            value (Integer/parseInt (second split-cmd))]
        (cond
          (= action "forward")  (recur (rest moves) (+ depth (* aim value)) aim           (+ position value))
          (= action "up")       (recur (rest moves) depth                   (- aim value) position)
          (= action "down")     (recur (rest moves) depth                   (+ aim value) position))))))


(def one (navigate input))
(def two (navigate-aim input))

(println "answers:")
(println one)
(println two)
