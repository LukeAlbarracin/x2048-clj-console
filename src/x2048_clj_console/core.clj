;; core.clj
(ns x2048-clj-console.core)

(def grid (atom (repeat 16 2)))

(defn print-grid []
  (partition 4 @grid))

(defn print-grid-vec [nums index] ;;(shift-grid (print-grid vec [] 0)0)
  (if (< index 3)
    (recur (conj nums (into [](nth (print-grid) index))) (inc index))
    (conj nums (into [] (nth (print-grid) index)))))

;;rotate-grid would be shift-up rotating in 270 or -90 degrees
(defn rotate-grid [nums index] ;;2d vector of the columns or back to rows
  (if (< index 3) ;;change @grid and add parameter to allow rotation
    (recur (conj nums (into [](take-nth 4 (drop index @grid)))) (inc index))
    (conj nums (into [] (take-nth 4 (drop index @grid))))))

(defn flip-grid [nums index] ;;change out print-grid-vec into params
  (if (< index 3)
    (recur (conj nums (into [](reverse (nth (print-grid-vec [] 0) index)))) (inc index))
    (conj nums (into [] (reverse (nth (print-grid-vec [] 0) index))))))

(defn add-zeros [nums]
 (concat nums (take (- 4 (count nums)) (cycle (range 0 1)))))

(defn filter-zeros [nums]
  (add-zeros (filter pos? nums))) 

(defn shift-grid [new-grid index]
  (if (< index 3)
      (recur (vec-algorithm new-grid index) (inc index))
      (vec-algorithm new-grid index)))

(defn vec-algorithm [new-grid index]
  (assoc new-grid index
  (iterate-thru (into [] (drop-while zero? (nth (print-grid) index)))0)))

(defn iterate-thru [nums index] ;;recursion
  (if (> (- (count nums) 1) index)
  (do ;;true
    (declare num-compare)
    (let [foo (num-compare (nth nums index) (nth nums (+ index 1)) nums index)]
      (recur foo (inc index))))
  (filter-zeros nums))) ;;false returns nums

(defn num-compare [x y nums index] ;;x is first, y is second
  (if (= x y)
    (update (update nums (+ index 1) #(* % 0)) index #(* % 2))
    nums))

(defn -main [& args]
  (println "Hello, 2048! :)"))
