;; core.clj
(ns x2048-clj-console.core)

(def grid (atom (repeat 16 0)))
(def score (atom 0))

(defn print-grid []
  (partition 4 @grid))

(defn print-grid-vec [nums index] ;;(shift-grid (print-grid-vec [] 0)0)
  (if (< index 3)
    (recur (conj nums (into [](nth (print-grid) index))) (inc index))
    (conj nums (into [] (nth (print-grid) index)))))

;;rotate-grid would be shift-up rotating in -270 or 90 degrees
;; use @grid for up
(defn rotate-grid [nums index temp-grid] ;;2d vector of the columns or back to rows
  (if (< index 3) ;;change @grid and add parameter to allow rotation
    (recur (conj nums (into [](take-nth 4 (drop index temp-grid)))) (inc index) temp-grid)
    (conj nums (into [] (take-nth 4 (drop index temp-grid))))))

;;use (print-grid-vec [] 0) for right, (rotate-grid ) for left
(defn flip-grid [nums index temp-grid] ;;change out print-grid-vec into params
  (if (< index 3)
    (recur (conj nums (into [](reverse (nth temp-grid index)))) (inc index) temp-grid)
    (conj nums (into [] (reverse (nth temp-grid index))))))

(defn add-zeros [nums]
 (concat nums (take (- 4 (count nums)) (cycle (range 0 1)))))

(defn filter-zeros [nums]
  (add-zeros (filter pos? nums))) 

(defn num-compare [x y nums index] ;;x is first, y is second
  (if (= x y) ;;doubles the first number and sets second number to 0 if true
    (do
      (reset! score (+ (* 2 x) @score))
      (update (update nums (+ index 1) #(* % 0)) index #(* % 2))) 
    nums))

(defn iterate-thru [nums index] ;;recursion
  (if (> (- (count nums) 1) index)
    (do ;;true
      (let [foo (num-compare (nth nums index) (nth nums (+ index 1)) nums index)]
        (recur foo (inc index))))
    (filter-zeros nums)))

(defn vec-algorithm [new-grid index]
  (assoc new-grid index
         (iterate-thru (into [] (filter pos? (nth new-grid index))) 0)))

(defn shift-grid [new-grid index]
  (declare vec-algorithm)
  (if (< index 3)
      (recur (vec-algorithm new-grid index) (inc index))
      (vec-algorithm new-grid index)))

(defn rotate-none [] ;;shift-left
  (into [] (print-grid)))

(defn rotate-half [] ;;shift-right 
  (flip-grid [] 0 (print-grid-vec [] 0)))

(defn rotate-small [] ;;-90 or 270 shift-down
  (flip-grid [] 0 (rotate-grid [] 0 @grid)))

(defn rotate-large [] ;;-270 or 90 shift-up
  (rotate-grid [] 0 @grid))

(defn add-block []
  (let [foo (rand-int 16)]
  (if (zero?(nth @grid foo))
    (reset! grid (assoc (into [] @grid) foo 2))
    (if (zero?(count (filter zero? (into [] @grid))))
      (println "Be very careful with your next move...")
      (recur)))))

(defn display-grid []
  (let [input (read-line)]
      (cond
        (= input "w") (reset! grid (flatten (rotate-grid [] 0 (flatten (shift-grid (rotate-large) 0)))))
        (= input "a") (reset! grid (flatten (shift-grid (rotate-none) 0)))
        (= input "s") (reset! grid (flatten (rotate-grid [] 0 (flatten (flip-grid [] 0 (shift-grid (rotate-small) 0))))))
        :else (reset! grid (flatten (flip-grid [] 0 (shift-grid (rotate-half) 0))))))
  (add-block)
  (doseq [foo (print-grid)] (println foo))
  (println "Score : " @score)
  (recur))

(defn -main [& args]
  (println "Hello, 2048! :)")
  (add-block)
  (add-block)
  (display-grid))
  
      




