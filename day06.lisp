
(defpackage #:advent2020.day06
  (:use #:cl #:alexandria #:advent2020.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day06)



(defun read-groups ()
  (with-puzzle-file (stream)
    (loop :with group := ()
          :for line := (read-line stream nil)
          :while line
          :if (string= line "")
            :collect (reverse group) :into groups
            :and :do (setf group ())
          :else
            :do (push line group)
          :finally (return (if group
                               (cons group groups)
                               groups)))))

(defparameter +input+ (read-groups))

(defparameter +qs+ "abcdefghijklmnopqrstuvwxyz")

(defun count-answers (group)
  (loop :with answered-qs := (make-hash-table :test 'equal)
        :for line :in group
        :do (loop :for c :across line
                  :do (setf (gethash c answered-qs) t))
        :finally (return (length (hash-table-keys answered-qs)))))

(defun solve-part-1 ()
  (reduce #'+ (mapcar 'count-answers +input+)))

(defun count-answers* (group)
  (loop :with missed-qs := (make-hash-table :test 'equal)
        :for line :in group
        :do (loop :for c :across line
                  :with answered-qs := (make-hash-table :test 'equal)
                  :do (setf (gethash c answered-qs) t)
                  :finally (loop :for q :across +qs+
                                 :unless (gethash q answered-qs)
                                   :do (setf (gethash q missed-qs) t)))
        :finally (return (loop :for q :across +qs+
                               :count (not (gethash q missed-qs))))))

(defun solve-part-2 ()
  (reduce #'+ (mapcar 'count-answers* +input+)))
