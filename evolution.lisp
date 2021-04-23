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

(defparameter *reproduction-energy* 200)

(defparameter *animals*
  (list (make-animal :x (ash *width* -1)
                     :y (ash *height* -1)
                     :energy 1000
                     :dir 0
                     :genes (loop repeat 8
                                  collecting (1+ (random 10)))))
  "All the animals.")

;;; Plants

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

;;; Animals

(defun move (animal)
  "Move ANIMAL based on its DIR."
  (let ((dir (animal-dir animal))
        (x (animal-x animal))
        (y (animal-y animal)))
    (setf (animal-x animal)
          (mod (+ x
                  (cond ((and (>= dir 2)
                              (< dir 5))
                         1)
                        ((or (= dir 1)
                             (= dir 5))
                         0)
                        (t -1))
                  *width*)
               *width*))
    (setf (animal-y animal)
          (mod (+ y
                  (cond ((and (>= dir 0)
                              (< dir 3))
                         -1)
                        ((and (>= dir 4)
                              (< dir 7))
                         0)
                        (t 0))
                  *height*)
               *height*))
    (decf (animal-energy animal))))

(defun turn (animal)
  "Turn ANIMAL based on its GENES."
  (let ((x (random (apply #'+
                          (animal-genes animal)))))
    (labels ((angle (genes x)
              (let ((x-nu (- x
                            (car genes))))
                (if (< x-nu 0)
                  0
                  (1+ (angle (cdr genes)
                             x-nu))))))
      (setf (animal-dir animal)
            (mod (+ (animal-dir animal)
                    (angle (animal-genes animal)
                           x))
                 8)))))

(defun eat (animal)
  "If there is a plant at ANIMAL's location, then ANIMAL eats it."
  (let ((pos (cons (animal-x animal)
                   (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal)
            *plant-energy*)
      (remhash pos *plants*))))

(defun reproduce (animal)
  "ANIMAL reproduces, creating a near clone of itself."
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      (setf (animal-energy animal)
            (ash e -1))
      (let ((animal-nu (copy-structure animal))
            (genes-nu (copy-list (animal-genes animal)))
            (mutation (random 8)))
        (setf (nth mutation genes-nu)
              (max 1
                    (+ (nth mutation genes-nu)
                       (random 3)
                       -1)))
        (setf (animal-genes animal-nu)
              genes-nu)
        (push animal-nu *animals*)))))

;;; World Update

(defun update-world ()
  "Update the entire worl."
  (setf *animals* (remove-if (lambda (animal)
                               (<= (animal-energy animal)
                                   0))
                             *animals*))
  (mapc (lambda (animal)
          (turn animal)
          (move animal)
          (eat animal)
          (reproduce animal))
        *animals*)
  (add-plants))

;;; Drawing

(defun draw-world ()
  "Draw the world to standard out."
  (loop for y
        below *height*
        do (progn (fresh-line)
                  (princ "|")
                  (loop for x
                        below *width*
                        do (princ (cond ((some (lambda (animal)
                                                 (and (= (animal-x animal)
                                                         x)
                                                      (= (animal-y animal)
                                                         y)))
                                               *animals*)
                                         #\M)
                                        ((gethash (cons x y)
                                                  *plants*)
                                         #\*)
                                        (t #\space))))
                  (princ "|"))))

(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") nil)
          (t (let ((x (parse-integer str :junk-allowed t)))
               (if x
                 (loop for i
                       below x
                       do (update-world)
                       if (zerop (mod i 1000))
                         do (princ #\.))
               (update-world))
             (evolution))))))

