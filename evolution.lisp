(defparameter *width* 100)

(defparameter *height* 50)

(defparameter *jungle* '(45 20 10 10) "Where the jungle is.")

(defparameter *plant-energy* 80)

(defparameter *plants* (make-hash-table :test #'equal) "Where the plants are.")

(defstruct animal
  x
  y
  energy
  dir
  genes)

(defparameter *animals*
  (list (make-animal :x (ash *width* -1)
                     :y (ash *height* -1)
                     :energy 1000
                     :dir 0
                     :genes (loop repeat 8
                                  collecting (1+ (random 10)))))
  "All the animals.")

(defun random-plant (left top width height)
  "Create a new plant in a region of the world."
  (let ((pos (cons (+ left
                      (random width))
                   (+ top
                      (random height)))))
    (setf (gethash pos *plants*)
          t)))

(defun add-plants ()
  "Add two plants to the world; one in the jungle and one in the world."
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))
