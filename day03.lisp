
(defpackage #:advent2020.day03
  (:use #:cl #:alexandria #:advent2020.util #:arrows)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day03)



(defun parse-map ()
  "Break the input file into a list of lines."
  (with-puzzle-file (stream)
    (loop :for line := (read-line stream nil)
          :while line
          :collect line)))

(defparameter +input+ (parse-map))

(defun map-width ()
  (length (first +input+)))

(defun map-height ()
  (length +input+))

(defun tree-at (x y)
  "Return whether there's a tree at X and Y."
  (char= #\#
         (char (nth y +input+) (mod x (map-width)))))

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
