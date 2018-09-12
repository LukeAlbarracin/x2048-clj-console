(ns x2048-clj-console.core) 

(def grid (atom (repeat 16 0))) 
(def score (atom 0))

(defn grid-2d
  "2D Representation of a Flattened Grid"
  ([] (mapv vec (partition 4 @grid)))
  ([temp-grid] (mapv vec (partition 4 temp-grid))))
  
(defn add-zeros [row]
  "Adds zeros if the row is too short"
 (concat row (take (- 4 (count row)) (cycle [0]))))

(defn filter-zeros [row] 
  "Removes zeros and nil before shifting grid"
  (add-zeros (filter pos? row))) 

(defn num-compare [row index] 
  "Compares the two numbers"
  (if (= (nth row index) (nth row (+ index 1) nil)) 
    (do
      (swap! score + (* 2 (nth row index 0)))
      (update (update row (+ index 1) #(* % 0)) index #(* % 2))) 
    row))

(defn update-row 
  "Returns an updated version of the row"
  ([row] (update-row row 0))
  ([row index]
    (if (> (count row) index)
      (recur (num-compare row index) (inc index))
      (filter-zeros row))))

(defn assoc-row
  "Associates a particular row with the grid"
  ([new-grid] (assoc-row new-grid 0))
  ([new-grid index] 
    (assoc new-grid index
      (update-row 
        (into [] (filter pos? (nth new-grid index)))))))

(defn shift-grid 
  "Returns the updated version of the whole grid"
  ([new-grid] (shift-grid new-grid 0))
  ([new-grid index]
    (if (< index 3)
        (recur (assoc-row new-grid index) (inc index))
        (assoc-row new-grid index))))

(defmacro back-and-forth 
  "Powerful macro that returns the grid to its natural state after rotating it"
  ([f1 f2] `(->> (grid-2d) ~@f1 ~@f2 (shift-grid) ~@f2 ~@f1 (flatten))))

(defn up-grid [temp-grid]
  "Logic for shifting the grid up ... Rotates it"
  (apply mapv vector temp-grid))

(defn right-grid [temp-grid]
  "Logic for shifting the grid right ... Rotates it"
  (mapv vec (mapv reverse temp-grid)))

(defn down-grid [temp-grid]
  "Logic for shifting the grid down ... Rotates it"
  (back-and-forth (up-grid) (right-grid)))

(defn add-block [] 
  "Adds a '2' square to the grid"
  (let [index (rand-int 16)]
  (if (zero?(nth @grid index nil))
    (reset! grid (assoc (into [] @grid) index 2))
    (if (zero? (count (filter zero? (into [] @grid))))
      (println "Be very careful with your next move...")
      (recur)))))

(defn display-grid [] 
  "DISPLAYS THE GRID AND TAKES IN INPUT RECURSIVELY"
  (println "--------")
  (doseq [temp-grid (grid-2d)] (println temp-grid))
  (let [input (clojure.string/lower-case (read-line))]
      (cond 
        (= input "w") (reset! grid (back-and-forth (up-grid) (identity)))
        (= input "a") (->> (grid-2d) (shift-grid) (flatten) (reset! grid))
        (= input "s") (reset! grid (down-grid (grid-2d)))
        (= input "d") (reset! grid (back-and-forth (right-grid) (identity)))
        :otherwise (println "Type in WASD please..."))) 
  (add-block)
  (println "--------")
  (println "Score : " @score)
  (recur))

(defn -main [& args]
  (println "This is 2048")
  (println "Enter a WASD key to move")
  (dotimes [i 2]
    (add-block))
  (display-grid))