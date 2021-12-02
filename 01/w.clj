(require '(clojure.string))

(def input (clojure.string/split-lines (slurp *in*)))

(def one
  (second (reduce (fn [acc value-str]
            (let [prev-val (first acc)
                  sum (second acc)
                  value (Integer/parseInt value-str)]
              (list value (+ sum (if (< prev-val value) 1 0)))))
          (list Integer/MAX_VALUE 0)
          input)))

(println one)
