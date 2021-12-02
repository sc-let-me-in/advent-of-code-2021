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

(def one (navigate input))

(println "answers:")
(println one)
