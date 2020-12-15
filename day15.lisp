
(defpackage #:advent2020.day15
  (:use #:cl #:alexandria #:advent2020.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day15)



(defparameter +input+ '(6 4 12 1 20 0 16))

(defun starting-memory (preamble)
  "Create a hash table of the last turn of every mentioned number."
  (loop :with result := (make-hash-table)
        :for i :from 1
        ;; reserve the last number for the next turn.
        :for x :in (butlast preamble)
        :do (setf (gethash x result) i)
        :finally (return result)))

(defun play-memory (preamble last-turn)
  (loop :with memory := (starting-memory preamble)
        :with current := (car (last preamble))

        :for turn :upfrom (length preamble) :below last-turn

        ;; Remember how many turns ago the current number was said.
        :for next := (- turn (gethash current memory turn))

        ;; Remember the current number for later.
        :do (setf (gethash current memory) turn
                  ;; Say the next number, making it the current number.
                  current next)
        :finally (return current)))

(defun solve-part-1 ()
  (play-memory +input+ 2020))

(defun solve-part-2 ()
  (play-memory +input+ 30000000))
