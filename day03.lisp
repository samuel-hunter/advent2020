
(defpackage #:advent2020.day03
  (:use #:cl #:alexandria #:advent2020.util #:arrows)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day03)



(defun parse-map ()
  "Convert the input file into a list of lists of booleans of whether a tree is at X,Y."
  (with-puzzle-file (stream)
    (loop :for line := (read-line stream nil)
          :while line
          :collect (loop :for char :across line
                         :collect (char= char #\#)))))

(defun map-array (thelist)
  "Convert the map list into a 2D array."
  (let ((map (make-array (list (length (first thelist)) (length thelist))
                         :element-type 'boolean
                         :initial-element nil)))
    (loop :for y :in thelist
          :for yi :upfrom 0
          :do (loop :for x :in y
                    :for xi :upfrom 0
                    :do (setf (aref map xi yi)
                              x)))
    map))

(defparameter +input+ (map-array (parse-map)))

(defun map-width ()
  (first (array-dimensions +input+)))

(defun map-height ()
  (second (array-dimensions +input+)))

(defun tree-at (x y)
  (aref +input+
        (mod x (map-width))
        y))

(defun count-trees (dx dy)
  (loop :for x :upfrom 0 :by dx
        :for y :upfrom 0 :below (map-height) :by dy
        :count (tree-at x y)))

(defun solve-part-1 ()
  (count-trees 3 1))

(defun solve-part-2 ()
  (* (count-trees 1 1)
     (count-trees 3 1)
     (count-trees 5 1)
     (count-trees 7 1)
     (count-trees 1 2)))
