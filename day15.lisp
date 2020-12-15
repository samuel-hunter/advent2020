
(defpackage #:advent2020.day15
  (:use #:cl #:alexandria #:advent2020.util #:split-sequence)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day15)



(defparameter +input+ '(6 4 12 1 20 0 16))

(defun solve-part-1 ()
  (loop :with memory := (reverse +input+)
        :for latest := (first memory)
        :for next := (1+ (or (position latest (rest memory))
                             -1))

        :repeat (- 2020 (length +input+))

        :do (print (list latest next))
        :do (push next memory)
        :finally (return latest)))

(defun starting-memory (input)
  (loop :with result := (make-hash-table)
        :for i :from 0
        :for x :in (butlast input)
        :do (setf (gethash x result) i)
        :finally (return result)))

(defun solve-part-2 ()
  (loop :with memory := (starting-memory +input+)
        :with current := (car (last +input+))
        :for prev := current
        :for turn :upfrom (1- (length +input+))
        :for next := (- turn (gethash current memory turn))
        :repeat (- 30000000 (length +input+))
        :do (setf (gethash current memory) turn
                  current next)
        :finally (return prev)))
