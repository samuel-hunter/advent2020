
(defpackage #:advent2020.day14
  (:use #:cl #:alexandria #:advent2020.util #:split-sequence)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day14)



(defun parse-line (line)
  (if (starts-with-subseq "mask" line)
      (loop :with mask-str := (third (split-sequence #\Space line))
            :with 0-mask := (make-array 36 :element-type 'bit)
            :with 1-mask := (make-array 36 :element-type 'bit)
            :with x-mask := (make-array 36 :element-type 'bit)
            :for bitstr :across mask-str
            :for i :upfrom 0
            :do (setf (bit (eswitch (bitstr)
                             (#\0 0-mask)
                             (#\1 1-mask)
                             (#\X x-mask))
                           i)
                      1)
            :finally (return (list :mask 0-mask 1-mask x-mask)))
      (list :mem (parse-integer (second (split-sequence #\[ line)) :junk-allowed t)
            (parse-integer (third (split-sequence #\Space line))))))

(defparameter +input+
  (parse-lines #'parse-line))

(defun 0-mask (mask)
  (second mask))

(defun 1-mask (mask)
  (third mask))

(defun x-mask (mask)
  (fourth mask))

(defun to-bit-array (num)
  (loop :with array := (make-array 36 :element-type 'bit)
        :for i :below 36
        :do (setf (bit array (- 36 i 1))
                  (mod (ash num (- i)) 2))
        :finally (return array)))

(defun to-num (bit-array)
  (loop :for i :below 36
        :for coef := 1 :then (* 2 coef)
        :sum (* (bit bit-array (- 36 i 1)) coef)))

(defun apply-mask (num mask)
  (to-num (bit-ior (bit-and (to-bit-array num) (bit-not (0-mask mask))) (1-mask mask))))

(defun solve-part-1 ()
  (loop :with mask := nil
        :with ram := (make-hash-table :test 'eql)
        :for action :in +input+
        :if (eq :mask (first action))
          :do (setf mask action)
        :else
          :do (setf (gethash (second action) ram)
                    (apply-mask (third action) mask))
        :finally
           (return (loop :for v :being :the :hash-values :of ram
                         :sum v))))

(define-condition floating-num-condition ()
  ((value :initarg :value :accessor value)))

(defun signal-floating-combinations (floating-num &optional partial-result (start 0))
  (loop :with result := (or partial-result (make-array 36 :element-type 'bit))
        :for i :from start :below 36
        :for (bit . rest-bits) :on floating-num
        :do (if (eq :x bit)
                (progn
                  (setf (bit result i) 0)
                  (signal-floating-combinations rest-bits result (1+ i))
                  (setf (bit result i) 1))
                (setf (bit result i) bit))
        :finally (signal 'floating-num-condition :value (to-num result))))

(defun collect-combinations (floating-num)
  (let (values)
    (handler-bind ((floating-num-condition (lambda (c)
                                             (push (value c) values))))
      (signal-floating-combinations floating-num))
    (reverse values)))

(defun apply-floating-mask (num mask)
  (loop :with num-array := (to-bit-array num)
        :for i :below 36
        :collect (cond
                   ((= 1 (bit (0-mask mask) i)) (bit num-array i))
                   ((= 1 (bit (1-mask mask) i)) 1)
                   ((= 1 (bit (x-mask mask) i)) :x))))

(defun solve-part-2 ()
  (loop :with mask := nil
        :with ram := (make-hash-table :test 'eql)
        :for action :in +input+
        :if (eq :mask (first action))
          :do (setf mask action)
        :else
          :do (loop :for address :in (collect-combinations
                                      (apply-floating-mask (second action)
                                                           mask))
                    :do (setf (gethash address ram) (third action)))
        :finally
           (return (loop :for v :being :the :hash-values :of ram
                         :sum v))))
