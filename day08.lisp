
(defpackage #:advent2020.day08
  (:use #:cl #:alexandria #:advent2020.util #:split-sequence)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day08)



(defun parse-instr (line)
  (destructuring-bind (op num)
      (split-sequence #\Space line)
    (list (make-keyword (string-upcase op))
          (parse-integer num))))

(defparameter +input+
  (coerce (parse-lines #'parse-instr) 'vector))

(defun instr-op (instr)
  (car instr))
(defun (setf instr-op) (new-value instr)
  (setf (car instr) new-value))

(defun evaluate (code)
  "Evaluate CODE and return a list of two values: the value of the
accumulator, and whether the program terminated normally."
  (loop ;; Keep track of all instructions that the evaluator has seen before.
        :with instrs := (make-hash-table :test 'equal)

        ;; op-code registers
        :with acc := 0
        :for isp := 0 :then (1+ isp)

        ;; Stop when the instruction was seen before, or if isp
        ;; reached the end of the code.
        :until (or (gethash isp instrs)
                   (= isp (length code)))
        :for (op num) := (aref code isp)

        :do (setf (gethash isp instrs) t) ;; remember this instruction for later.
            ;; interpret the op-code.
            (ecase op
              (:acc (incf acc num))
              (:jmp (incf isp (1- num)))
              (:nop))
        :finally (return (list acc
                               (= isp (length code))))))

(defun solve-part-1 ()
  (first (evaluate +input+)))

(defun find-swappable (code &optional (start 0))
  "Return the index of the next swappable instruction."
  (loop :for i :from start :to (length code)
        :until (member (instr-op (aref code i)) '(:jmp :nop))
        :finally (return i)))

(defun swap-instr (code indx)
  (let ((instr (aref code indx)))
    (setf (instr-op instr)
          (ecase (instr-op instr)
            (:nop :jmp)
            (:jmp :nop)))))

(defun solve-part-2 ()
  ;; Keep swapping jmp/nop instructions until the code terminated
  ;; normally.
  (loop :with code := (copy-seq +input+)
        :for i := (find-swappable code)
          :then (find-swappable code (1+ i))
        :for (acc terminated-normally)
          := (prog2
                 (swap-instr code i)
                 (evaluate code)
               (swap-instr code i))

        :until terminated-normally
        :finally (return acc)))
