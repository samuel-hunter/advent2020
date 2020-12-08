
(defpackage #:advent2020.day08
  (:use #:cl #:alexandria #:advent2020.util #:arrows #:split-sequence)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day08)



(defparameter +input+ (coerce (read-puzzle-sexp "day08.lisp")
                              'vector))

(defun solve-part-1 (&optional (code +input+))
  (loop :with accumulator := 0
        :with instrs := (make-hash-table :test 'equal)
        :for i := 0 :then (1+ i)
        :until (or (gethash i instrs)
                   (= i (array-dimension code 0)))
        :for (op num) := (aref code i)

        :do (print (list i op num))
        :do (setf (gethash i instrs) t)
            (ecase op
                 (:acc (incf accumulator num))
                 (:jmp (incf i (1- num)))
                 (:nop))
        :finally (return accumulator)))

(defun evaluate (code)
  (loop :with accumulator := 0
        :with instrs := (make-hash-table :test 'equal)
        :for i := 0 :then (1+ i)
        :for (op num) := (when (< i (array-dimension code 0))
                           (aref code i))
        :until (gethash i instrs)

        :when (= i (array-dimension code 0))
          :do (return t)

        :do (list i op num)
        :do (setf (gethash i instrs) t)
            (ecase op
                 (:acc (incf accumulator num))
                 (:jmp (incf i (1- num)))
                 (:nop))))

(defun swap-instr (code indx)
  (let ((instr (aref code indx)))
    (print (list :swap indx instr))
    (setf (car instr)
          (ecase (car instr)
            (:nop :jmp)
            (:jmp :nop)))))

(defun solve-part-2 ()
  (let ((code (copy-seq +input+)))
    (loop :for i :below (array-dimension code 0)
          :for instr := (aref code i)
          :when (member (car instr) '(:jmp :nop))
            :do (progn
                  (print (list :loop i instr)))
                  (swap-instr code i)
                  (when (evaluate code)
                    (return (solve-part-1 code)))
                  (swap-instr code i))))
