(ns matrix-symmetricity.core
  (:gen-class))

(def symmetric "symmetric")
(def not-symmetric "not symmetric")
(def not-possible "not possible")

(defn one-times-one-matrix?
  "Checks if the given matrix is 1x1."
  [matrix]
  (= (count matrix) 1))

(defn not-square-matrix?
  "Checks if the given matrix is not a square matrix by checking
   the amount of rows and the amount of characters in a row. Assumes
   that the matrices do not have uneven rows."
  [matrix]
  (not= (count (get matrix 0)) (count matrix)))

(defn generate-coordinates
  "Generates a vector of vectors using recursion. Ignores pairs
   that are irrelevant for checking symmetricity."
  [{:keys [x y length pairs]}]
  (cond
    (= x y length) pairs
    (= x length)   (generate-coordinates {:x      (inc y)
                                          :y      (inc y)
                                          :length length
                                          :pairs  (conj pairs {:x x :y y})})
    (= x y)        (generate-coordinates {:x      (inc x)
                                          :y      y
                                          :length length
                                          :pairs  pairs})
    :else          (generate-coordinates {:x      (inc x)
                                          :y      y
                                          :length length
                                          :pairs  (conj pairs {:x x :y y})})))

(defn symmetric?
  "Checks symmetricity by going through all relevant x-y -pairs 
   in the matrix. Relevant pairs are generated, and then
   in a 3x3 matrix for example pairs (x=0, y=2) and (x=2, y=0) 
   are compared."
  [matrix]
  (let [pairs (generate-coordinates {:x      1
                                     :y      0
                                     :length (- (count matrix) 1)
                                     :pairs  []})]
    (every? #(= (get-in matrix [(:x %) (:y %)])
                (get-in matrix [(:y %) (:x %)])) pairs)))

(defn insert-character
  "Inserts a new character to a row. If the row doesn't exist 
  it also creates a new row."
  [{:keys [matrix character row-number]}]
  (let [matrix-row (get matrix row-number)]
    (if (nil? matrix-row)
      (conj matrix [character])
      (assoc-in matrix [row-number] (into matrix-row [character])))))

(defn str-arr->matrix
  "Takes the vector given in the task, returns a vector in which vectors 
   within it represents the rows of the matrix."
  [strArr]
  (reduce
   (let [row-number (atom 0)]
     (fn [matrix character]
       (if (not= character "<>")
         (insert-character {:character  character
                            :matrix     matrix
                            :row-number @row-number})
         (do
           (swap! row-number inc)
           matrix))))
   []
   strArr))

(defn matrix-symmetricity
  [str-arr]
  (let [matrix (str-arr->matrix str-arr)]
    (cond
      (one-times-one-matrix? matrix) symmetric
      (not-square-matrix? matrix)    not-possible
      (symmetric? matrix)            symmetric
      :else                          not-symmetric)))
