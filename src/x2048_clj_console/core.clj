(ns x2048-clj-console.core) ;; NS STANDS FOR NAMESPACE IN CLOJURE

(def grid (atom (repeat 16 0))) ;; ATOM IS A MUTABLE VARIABLE - VARIABLES IN CLOJURE ARE IMMUTABLE BY DEFAULT
(def score (atom 0))

(defn print-grid-vec [] ;; DEFAULT : LEFT GRID - GETS AN INSTANCE OF 4 ROWS OF THE GRID
  (mapv vec (partition 4 @grid)))
  
(defn reverse-down-grid [nums] ;; DISCLAIMER : HARDCODED VERSION - REVERTS DOWN-GRID BACK TO DEFAULT LEFT STATE
  (apply mapv vector (mapv reverse nums)))

(defn add-zeros [nums] ;; ADDS ZEROS IF THE ROW IS TOO SHORT
 (concat nums (take (- 4 (count nums)) (cycle (range 0 1)))))

(defn filter-zeros [nums] ;; REMOVES ZEROS TO ALLOW SQUARES WITH VALUES TO SHIFT IN THEIR RESPECTIVE DIRECTION
  (add-zeros (filter pos? nums))) 

(defn num-compare [nums index] ;; COMPARES TWO NUMBERS AND APPLIES FUNCTIONS IF TRUE
  (if (= (nth nums index) (nth nums (+ index 1))) 
    (do
      (reset! score (+ (* 2 (nth nums index)) @score)) ;; UPDATES SCORE BY UPDATING THE ATOM USING "reset!"
      (update (update nums (+ index 1) #(* % 0)) index #(* % 2))) ;; DOUBLES THE FIRST NUMBER, MAKES SECOND NUMBER ZERO
    nums))

(defn iterate-thru [nums index] ;; ITERATES THRU A PARTICULAR ROW, USING THE ABOVE FUNCTION AS KEY LOGIC
  (if (> (- (count nums) 1) index)
    (do 
      (let [foo (num-compare nums index)]
        (recur foo (inc index))))
    (filter-zeros nums)))

(defn vec-algorithm [new-grid index] ;; RETURNS AN UPDATED VERSION OF A ROW/COLUMN W/O MUTATING IT USING THE ABOVE METHODS
  (assoc new-grid index
         (iterate-thru (vec (filter pos? (nth new-grid index)))0)))

(defn shift-grid [new-grid index] ;; RETURNS AN UPDATED VERSION OF THE WHOLE GRID WITHOUT MUTATING IT
  (if (< index 3)
      (recur (vec-algorithm new-grid index) (inc index))
      (vec-algorithm new-grid index)))

(defn up-grid
  ([nums] (apply mapv vector nums))
  ([] (as-> (print-grid-vec) x
        (up-grid x)
        (shift-grid x 0)
        (up-grid x)
        (flatten x)
        (reset! grid x))))

(defn right-grid
  ([nums] (mapv vec (mapv reverse nums)))
  ([] (as-> (print-grid-vec) x
        (right-grid x)
        (shift-grid x 0)
        (right-grid x)
        (flatten x)
        (reset! grid x))))

(defn down-grid
  ([nums] (mapv reverse (apply mapv vector nums)))
  ([] (as-> (print-grid-vec) x
        (down-grid x)
        (shift-grid x 0)
        (reverse-down-grid x)
        (flatten x)
        (reset! grid x))))

(defn add-block [] ;; ADDS A "2" SQUARE TO THE GRID. RECURS IF THE "2" SQUARE WOULD BE ADDED TO AN OCCUPIED SQUARE
  (let [foo (rand-int 16)]
  (if (zero?(nth @grid foo))
    (reset! grid (assoc (into [] @grid) foo 2))
    (if (zero?(count (filter zero? (into [] @grid))))
      (println "Be very careful with your next move...") ;; WARNS THE USER THAT THEIR NEXT MOVE CAN RESULT IN A LOSS
      (recur))))) ;; RECURS IF FALSE

(defn display-grid [] ;; DISPLAYS THE GRID AND TAKES IN INPUT RECURSIVELY
  (doseq [foo (print-grid-vec)] (println foo))
  (let [input (read-line)]
      (cond ;; A STRANGE ATTEMPT TO REVERSE THE EFFECTS OF THE UPDATED INSTANCE OF A ROTATED/NON-ROTATED GRID
        (= input "w") (up-grid)
        (= input "a") (as-> (print-grid-vec) x (shift-grid x 0) (flatten x) (reset! grid x))
        (= input "s") (down-grid)
        (= input "d") (right-grid)
        :otherwise (println "Type in WASD please..."))) ;; KEYWORD OTHERWISE IS ALWAYS EVALUATED TO TRUTHY AS IT IS NEITHER NIL OR FALSE
  (add-block)
  (println "Score : " @score)
  (recur))

(defn -main [& args] ;; THE MAIN METHOD - WHERE ALL THE ABOVE CODE COMES INTO FRUITION
  (println "Hello, 2048! :)")
  (dotimes [i 2]
    (add-block))
  (display-grid))
