(ns x2048-clj-console.core-test
  (:require [clojure.test :refer :all]
            [x2048-clj-console.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

"
(defn shift-grid [new-grid index]
     (cond index
    (= index 0) (recur (vec-algorithm print-grid-vec index) (inc index))
    (< index 3) (recur (vec-algorithm new-grid index) (inc index))
      :else (vec-algorithm new-grid index)
     )"
(comment "
(defn move-grid [new-grid index]
  (let [foo (assoc (into [] (print-grid)) index
                   (declare iterate-thru)
                   (iterate-thru (into [] (drop-while zero? (nth (print-grid-vec) index))) 0))]
    (if (< index 3)
      (recur foo (inc index))
      (add-zeros foo))))")

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
