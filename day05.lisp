
(defpackage #:advent2020.day05
  (:use #:cl #:alexandria #:advent2020.util #:arrows #:split-sequence)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day05)



(defparameter +input+ (parse-lines #'identity))

(defun row-number (line)
  (loop :for major := 64 :then (/ major 2)
        :for c :across (subseq line 0 7)
        :sum (if (char= #\F c)
                 0
                 major)))

(defun col-number (line)
  (loop :for major := 4 :then (/ major 2)
        :for c across (subseq line 7)
        :sum (if (char= #\R c)
                 major
                 0)))

(defun seat-id (line)
  (+ (* (row-number line) 8)
     (col-number line)))

(defun solve-part-1 ()
  (loop :for line :in +input+
        :maximize (seat-id line)))

(defun solve-part-2 ()
  (let ((seats (make-hash-table :test 'equal)))
    (loop :for line :in +input+
          :do (setf (gethash (seat-id line) seats) t))
    (loop :for id :upto (solve-part-1)
          :unless (gethash id seats)
            :do (print id))))
