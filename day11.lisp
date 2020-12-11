
(defpackage #:advent2020.day11
  (:use #:cl #:alexandria #:advent2020.util #:org.tfeb.hax.memoize)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day11)



(defun parse-grid (lines)
  (loop :with grid := (make-array (list (length (first lines))
                                        (length lines))
                                  :element-type 'character
                                  :initial-element #\.)
        :for line :in lines
        :for y :upfrom 0
        :do (loop :for c :across line
                  :for x :upfrom 0
                  :do (setf (aref grid x y) c))
        :finally (return grid)))

(defparameter +input+ (parse-grid (parse-lines #'identity)))

(defun width (grid)
  (array-dimension grid 0))
(defun height (grid)
  (array-dimension grid 1))

(defun seat (grid x y)
  (if (and (<= 0 x (1- (width grid)))
           (<= 0 y (1- (height grid))))
      (aref grid x y)
      #\L))
(defun (setf seat) (new-value grid x y)
  (setf (aref grid x y) new-value))

(defun occupied-p (char)
  (char= #\# char))
(defun empty-p (char)
  (char= #\L char))

(defparameter +adjacents+
  '((-1 -1)
    (-1  0)
    (-1 +1)

    ( 0 -1)
    ( 0 +1)

    (+1 -1)
    (+1  0)
    (+1 +1)))

(defun adjacents (grid x y)
  "Return all occupied adjacent seats."
  (loop :for (dx dy) :in +adjacents+
        :count (occupied-p (seat grid (+ x dx) (+ y dy)))))

(defun first-seat (grid origin-x origin-y dx dy)
  "Look from the origin to a direction for the first seat, and return its value."
  (loop :for x := (+ origin-x dx) :then (+ x dx)
        :for y := (+ origin-y dy) :then (+ y dy)
        :while (and (<= 0 x (1- (width grid)))
                    (<= 0 y (1- (height grid))))
        :for seat := (aref grid x y)
        :unless (char= seat #\.)
          :do (return (values seat t))
        :finally (return (values #\. nil))))

(defun adjacents* (grid x y)
  "Return all occupied adjacent seats, looking ahead to the nearest seat."
  (loop :for (dx dy) :in +adjacents+
        :count (occupied-p (first-seat grid x y dx dy))))

(defun eval-grid (grid adjacents tolerance)
  "Evaluate the grid and return a copy of the next generation, with
ADJACENTS being the function to count the adjacent occupied seats, and
TOLERANCE being the number of adjacent occupied seats to transform an
occupied seat to empty."
  (loop :with target := (copy-array grid)
        :for x :below (width grid)
        :do (loop :for y :below (height grid)
                  :for seat := (seat grid x y)
                  :do (cond ((and (empty-p seat)
                                  (= 0 (funcall adjacents grid x y)))
                             (setf (seat target x y) #\#))
                            ((and (occupied-p seat)
                                  (>= (funcall adjacents grid x y)
                                      tolerance))
                             (setf (seat target x y) #\L))))
        :finally (return target)))

(defun solve-grid (adjacents tolerance)
  (loop :for start := (copy-array +input+) :then next
        :for next := (eval-grid start adjacents tolerance)
        :until (equalp start next)
        :finally (return
                   (loop :for x :below (width next)
                         :sum (loop :for y :below (height next)
                                    :count (occupied-p (seat next x y)))))))

(defun solve-part-1 ()
  (solve-grid #'adjacents 4))

(defun solve-part-2 ()
  (solve-grid #'adjacents* 5))
