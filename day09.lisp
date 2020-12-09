
(defpackage #:advent2020.day09
  (:use #:cl #:alexandria #:advent2020.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day09)



(defparameter +preamble+ 25)
(defparameter +input+ (coerce (parse-lines #'parse-integer) 'vector))


(defun valid-p (index)
  "Return whether an index is valid -- that is, whether there is a
pair of numbers some addresses behind that adds up to this number."
  (loop :with num := (aref +input+ index)
        :for i1 :from (- index +preamble+) :below (1- index)
        :for v1 := (aref +input+ i1)
          :thereis (loop :for i2 :from (1+ i1) :below index
                         :for v2 := (aref +input+ i2)
                           :thereis (= num (+ v1 v2)))))

(defun solve-part-1 ()
  (loop :for i :from +preamble+ :below (length +input+)
        :while (valid-p i)
        :finally (return (aref +input+ i))))

(defun sums-to (num start)
  "Return a list of whether a series of numbers starting at START adds
up to NUM, and the end of this subsequence."
  (loop :for i :upfrom start
        :sum (aref +input+ i) :into acc
        :until (>= acc num)
        :finally (return (list (= acc num) (1+ i)))))

(defun solve-part-2 ()
  (loop :with num := (solve-part-1)
        :for start :below (length +input+)
        :for (sums-p end) := (sums-to num start)
        :until sums-p
        :finally (let ((seq (subseq +input+ start end)))
                   (return (+ (reduce #'max seq)
                       (reduce #'min seq))))))
