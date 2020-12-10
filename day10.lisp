
(defpackage #:advent2020.day10
  (:use #:cl #:alexandria #:advent2020.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day10)



(defparameter +input+ (sort (parse-lines #'parse-integer) #'<))

(defun solve-part-1 ()
  (loop :with diffs := (make-hash-table :test 'eql)
        :for (a b) :on +input+
        :while b
        :do (incf (gethash (- b a) diffs 0))
        :finally (return (* (1+ (gethash 3 diffs))
                            (1+ (gethash 1 diffs))))))



(defun arrangements (vals)
  (when (= 1 (length vals))
    (return-from arrangements 1))

  (loop :with start := (first vals)
        :for (val . rest) :on (rest vals)
        :while (<= (- val start) 3)
        :sum (arrangements* (cons val rest))))

(defparameter +arrangements+ (make-hash-table :test 'equalp))
(defun arrangements* (vals)
  (if (gethash vals +arrangements+)
      (gethash vals +arrangements+)
      (setf (gethash vals +arrangements+) (arrangements vals))))

(defun solve-part-2 ()
  (arrangements* (cons 0 +input+)))
