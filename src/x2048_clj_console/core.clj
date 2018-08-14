(ns x2048-clj-console.core) ;; NS STANDS FOR NAMESPACE IN CLOJURE

(def grid (atom (repeat 16 0))) ;; ATOM IS A MUTABLE VARIABLE - VARIABLES IN CLOJURE ARE IMMUTABLE BY DEFAULT
(def score (atom 0))

(defn print-grid-vec [] ;; DEFAULT : LEFT GRID - GETS AN INSTANCE OF 4 ROWS OF THE GRID
  (mapv vec (partition 4 @grid)))
  
(defn up-grid [nums] ;; ROTATES GRID 90 DEGREES COUNTERCLOCKWISE / REFLECTS GRID VERTICALLY
  (apply mapv vector nums)) ;; GETS AN INSTANCE OF 4 COLUMNS OF THE GRID

(defn right-grid [nums] ;; ROTATES GRID 180 DEGREES / REFLECTS GRID HORIZONTALLY
  (mapv vec (mapv reverse nums)))

(defn down-grid [nums] ;; ROTATES GRID 90 DEGREES CLOCKWISE / REFLECTS GRID BOTH HORIZONTALLY AND VERTICALLY
  (mapv reverse (apply mapv vector nums))) 

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
        (= input "w") (reset! grid (flatten (up-grid (shift-grid (up-grid (print-grid-vec))0)))) 
        (= input "a") (reset! grid (flatten (shift-grid (print-grid-vec)0)))
        (= input "s") (reset! grid (flatten (reverse-down-grid (shift-grid (down-grid (print-grid-vec))0))))
        (= input "d") (reset! grid (flatten (right-grid (shift-grid (right-grid (print-grid-vec))0))))
        :otherwise (println "Type in WASD please..."))) ;; KEYWORD OTHERWISE IS ALWAYS EVALUATED TO TRUTHY AS IT IS NEITHER NIL OR FALSE
  (add-block)
  (println "Score : " @score)
  (recur))

(defn -main [& args] ;; THE MAIN METHOD - WHERE ALL THE ABOVE CODE COMES INTO FRUITION
  (println "Hello, 2048! :)")
  (dotimes [i 2]
    (add-block))
  (display-grid))
