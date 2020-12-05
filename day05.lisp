
(defpackage #:advent2020.day05
  (:use #:cl #:alexandria #:advent2020.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day05)



(defparameter +input+ (parse-lines #'identity))

(defconstant +rows+ 128)
(defconstant +cols+ 8)

(defun row-number (line)
  ;; There are 128 rows, and the possible range halves by 2 for each
  ;; binary step to take. So, by adding up the current minimum
  ;; possible row until the range is 1, we eventually get our target
  ;; row.
  (loop :for range := (/ +rows+ 2) :then (/ range 2)
        :for c :across (subseq line 0 7)
        ;; Higher vales are at the back: #\B
        :sum (if (char= #\B c) range 0)))

(defun col-number (line)
  ;; Same algorithm here.
  (loop :for range := (/ +cols+ 2) :then (/ range 2)
        :for c across (subseq line 7)
        ;; Higher values are at the right: #\R
        :sum (if (char= #\R c) range 0)))

(defun seat-id (line)
  (+ (* (row-number line) +cols+)
     (col-number line)))

(defun solve-part-1 ()
  ;; Convert all lines into Seat ID's and return the maximum value.
  (reduce #'max (mapcar #'seat-id +input+)))

(defun solve-part-2 ()
  (let ((existing-ids (make-hash-table :test 'equal)))
    ;; Populate a set of existing seat IDs.
    (loop :for line :in +input+
          :do (setf (gethash (seat-id line) existing-ids) t))
    ;; Part 2's puzzle says seat ID's can be cut from the front and
    ;; back. It hints that our (missing) ID will always be surrounded
    ;; by two existing ID, but we don't have to follow this
    ;; hint. Instead, ranging through only the minimum and maximum
    ;; existing ID reveals our ID to be the sole missing one.
    (loop :for id :from (reduce #'min (mapcar #'seat-id +input+))
            :to (reduce #'max (mapcar #'seat-id +input+))
          :unless (gethash id existing-ids)
            :do (return id))))
