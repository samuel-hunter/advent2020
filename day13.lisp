
(defpackage #:advent2020.day13
  (:use #:cl #:alexandria #:advent2020.util #:split-sequence)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day13)



(destructuring-bind (time schedule) (parse-lines #'identity)
  (defparameter +earliest-time+ (parse-integer time))
  (defparameter +schedule+
    (loop :for x :in (split-sequence #\, schedule)
          :for num := (parse-integer x :junk-allowed t)
          :when num
            :collect num)))

(defun solve-part-1 ()
  (loop :for i :upfrom +earliest-time+
        :do (loop :for bus :in +schedule+
                  :when (= 0 (mod i bus))
                    :do (return-from solve-part-1
                          (* bus (* (- i +earliest-time+)))))))

(defun solve-part-2 ())
