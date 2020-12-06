
(defpackage #:advent2020.day06
  (:use #:cl #:alexandria #:advent2020.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day06)



(defparameter +input+ (parse-forms #'identity))

(defun count-any-answered (group)
  "Count teh number of questions that any people in GROUP answered."
  (loop :with answered-qs := (make-hash-table :test 'equal)
        :for line :in group
        :do (loop :for c :across line
                  :do (setf (gethash c answered-qs) t))
        :finally (return (length (hash-table-keys answered-qs)))))

(defun solve-part-1 ()
  ;; Count the questions for each group and sum them up.
  (reduce #'+ (mapcar 'count-any-answered +input+)))

(defun count-all-answered (group)
  "Count the number of questions that all people in GROUP answered."
  ;; Count the number of questions people missed
  (loop :with missed-qs := (make-hash-table :test 'equal)
        :for line :in group
        :do (loop :for c :across line
                  ;; Count all the questions answered
                  :with answered-qs := (make-hash-table :test 'equal)
                  :do (setf (gethash c answered-qs) t)
                      ;; And then touch to MISSED-QS all the questions missed.
                  :finally (loop :for code :from (char-code #\a) :to (char-code #\z)
                                 :for c := (code-char code)
                                 :unless (gethash c answered-qs)
                                   :do (setf (gethash c missed-qs) t)))
            ;; Finally, fold and invert.
        :finally (return (- 26 (length (hash-table-keys missed-qs))))))

(defun solve-part-2 ()
  ;; Count the questions for each group and sum them up.
  (reduce #'+ (mapcar 'count-all-answered +input+)))
