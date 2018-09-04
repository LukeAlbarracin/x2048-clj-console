(ns x2048-clj-console.core) ;; NS STANDS FOR NAMESPACE IN CLOJURE

(def grid (atom (repeat 16 0))) ;; ATOM IS A MUTABLE VARIABLE - VARIABLES IN CLOJURE ARE IMMUTABLE BY DEFAULT
(def score (atom 0))

(defn print-grid-vec 
  "DEFAULT : LEFT GRID - GETS AN INSTANCE OF 4 ROWS OF THE GRID"
  ([] (mapv vec (partition 4 @grid)))
  ([temp-grid] (mapv vec (partition 4 temp-grid))))
  
(defn reverse-down-grid [nums] 
  "Hardcoded method that reverts down-grid back to standard position"
  (apply mapv vector (mapv reverse nums)))

(defn add-zeros [nums]
  "Adds zeros if the row is too short"
 (concat nums (take (- 4 (count nums)) (cycle (range 0 1)))))

(defn filter-zeros [nums] 
  "Removes zeros before shifting grid"
  (add-zeros (filter pos? nums))) 

(defn num-compare [nums index] 
  "Compares the two numbers"
  (if (= (nth nums index) (nth nums (+ index 1))) 
    (do
      (reset! score (+ (* 2 (nth nums index)) @score)) ;; UPDATES SCORE BY UPDATING THE ATOM USING "reset!"
      (update (update nums (+ index 1) #(* % 0)) index #(* % 2))) ;; DOUBLES THE FIRST NUMBER, MAKES SECOND NUMBER ZERO
    nums))

(defn iterate-thru [nums index]
  "Iterates through a particular row"
  (if (> (- (count nums) 1) index)
    (do 
      (let [foo (num-compare nums index)]
        (recur foo (inc index))))
    (filter-zeros nums)))

(defn vec-algorithm [new-grid index] 
  "Returns the updated version of the row"
  (assoc new-grid index
         (iterate-thru (vec (filter pos? (nth new-grid index)))0)))

(defn shift-grid [new-grid index] 
  "Returns the updated version of the whole grid"
  (if (< index 3)
      (recur (vec-algorithm new-grid index) (inc index))
      (vec-algorithm new-grid index)))

(defn up-grid
  "Logic for shifting the grid up ... Rotates it"
  ([nums] 
    (apply mapv vector nums))
  ([temp-grid z] (as-> temp-grid x
        (up-grid x)
        (shift-grid x 0)
        (up-grid x)
        (flatten x))))

(defn right-grid
  "Logic for shifting the grid right ... Rotates it"
  ([nums] (mapv vec (mapv reverse nums)))
  ([temp-grid z] (as-> temp-grid x
        (right-grid x)
        (shift-grid x 0)
        (right-grid x)
        (flatten x))))

(defn down-grid
  "Logic for shifting the grid down ... Rotates it"
  ([nums] (mapv reverse (apply mapv vector nums)))
  ([temp-grid z] (as-> temp-grid x
        (down-grid x)
        (shift-grid x 0)
        (reverse-down-grid x)
        (flatten x))))

(defn add-block [] 
  "Adds a '2' square to the grid"
  (let [foo (rand-int 16)]
  (if (zero?(nth @grid foo))
    (reset! grid (assoc (into [] @grid) foo 2))
    (if (zero? (count (filter zero? (into [] @grid))))
      (println "Be very careful with your next move...")
      (recur)))))


(defn display-grid [] 
  "DISPLAYS THE GRID AND TAKES IN INPUT RECURSIVELY"
  (doseq [foo (print-grid-vec)] (println foo))
  (let [input (read-line)]
      (cond ;; A STRANGE ATTEMPT TO REVERSE THE EFFECTS OF THE UPDATED INSTANCE OF A ROTATED/NON-ROTATED GRID
        (= input "w") (reset! grid (up-grid (print-grid-vec) 0))
        (= input "a") (as-> (print-grid-vec) x (shift-grid x 0) (flatten x) (reset! grid x))
        (= input "s") (reset! grid (down-grid (print-grid-vec) 0))
        (= input "d") (reset! grid (right-grid (print-grid-vec) 0))
        :otherwise (println "Type in WASD please..."))) ;; KEYWORD OTHERWISE IS ALWAYS EVALUATED TO TRUTHY AS IT IS NEITHER NIL OR FALSE
  (add-block)
  (println "Score : " @score)
  (recur))

(defn -main [& args] ;; THE MAIN METHOD - WHERE ALL THE ABOVE CODE COMES INTO FRUITION
  (println "Hello, 2048! :)")
  (dotimes [i 2]
    (add-block))
  (display-grid))
