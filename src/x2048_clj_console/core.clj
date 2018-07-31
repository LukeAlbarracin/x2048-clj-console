;; core.clj
(ns x2048-clj-console.core)

(defn print-grid [grid]
  (partition 4 @grid)
)

(defn move-grid [new-grid index]
    (let [foo (assoc (into [] (print-grid)) index 
    (declare iterate-thru)
    (iterate-thru (drop-while zero? (nth (print-grid) index))0))]
      (if (< index 3)
        (recur foo (inc index))
        foo)))

(defn iterate-thru [nums index] ;;recursion
  (if (> (- (count nums) 1) index)
  (do ;;true
    (declare num-compare)
    (let [foo (num-compare (nth nums index) (nth nums (+ index 1)) nums index)]
      (recur foo (inc index))))
  nums)) ;;false returns nums

(defn num-compare [x y nums index] ;;x is first, y is second
  (if (= x y)
    (update (update nums (+ index 1) #(* % 0)) index #(* % 2))
    nums
  )
)

(comment "
(defn print-columns 
  ([] (let [foo (repeat 4 (list nil nil nil nil))] 
  (recur (print-grid grid))))
  ([new-grid]
    (let [foo (list (loop-columns (range 0 4)))]
    foo)))
 
(defn loop-columns [index]
  (filter (= (sort-for-columns (range 0 16)) index))
)
(defn pair-column-with-index [index num] ;;num = new index
  (cond num
    (> 1 (/ index 4)) 0
    (> 2 (/ index 4)) 1
    (> 3 (/ index 4)) 2
    : else 3))

(defn sort-for-columns [index] 
  (cond
    (zero? (mod index 4)) 0
    (zero? (mod (- index 1)4)) 1
    (zero? (mod (- index 2)4)) 2
    :else 3))
")

(defn -main [& args]
  (println "Hello, 2048! :)")
  ;;(def grid (repeat 4 (list 0 0 0 0)))
  (def grid (atom(repeat 16 2))))
