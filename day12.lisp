
(defpackage #:advent2020.day12
  (:use #:cl #:alexandria #:advent2020.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day12)



(defun parse-direction (line)
  (list
   (eswitch ((char line 0) :test 'char=)
     (#\W :west)
     (#\E :east)
     (#\N :north)
     (#\S :south)
     (#\R :right)
     (#\L :left)
     (#\F :forward)
     (#\B :backward))
   (parse-integer (subseq line 1))))

(defparameter +input+ (parse-lines #'parse-direction "example.txt"))

(defparameter +directions+ '(:north :east :south :west))

(defun turn (direction right-turns)
  (nth (mod (+ right-turns (position direction +directions+))
            (length +directions+))
       +directions+))

(defun pos-change (direction)
  (nth (position direction +directions+)
       +positions+))

(defun relative-dp (orientation direction)
  (nth (+ (position orientation '(:north :east :south :west))
          (position direction '(:forward :right :backward :left)))
       '((0 -1) (1 0) (0 1) (-1 0))))

(defun solve-part-1 ()
  (loop :with orientation := :east
        :with x := 0
        :with y := 0
        :for (action movement) :in +input+
        :do (ecase action
              ((:north :south :east :west)
               (destructuring-bind (dx dy)
                   (relative-dp action :forward)
                 (incf x (* dx movement))
                 (incf y (* dy movement))))
              ((:forward :backward)
               (destructuring-bind (dx dy)
                   (relative-dp orientation action)
                 (incf x (* dx movement))
                 (incf y (* dy movement))))
              ((:left :right)
               (setf orientation (turn orientation (* (if (eq :right action)
                                                          1 -1)
                                                      (/ movement 90))))))
        :finally (return (+ (abs x) (abs y)))))

(defun turn* (x y direction angle)
  (ecase (mod (* (if (eq :right direction) -1 1)
                 (/ angle 90))
              4)
    (0 (list x y))
    (1 (list y (- x)))
    (2 (list (- x) (- y)))
    (3 (list (- y) x))))

(defun solve-part-2 ()
  (loop :with orientation := :east
        :with x := 0
        :with y := 0
        :with wx := 10
        :with wy := -1
        :for (action movement) :in +input+
        :do (ecase action
              ((:north :east :south :west)
               (destructuring-bind (dx dy)
                   (relative-dp action :forward)
                 (incf wx (* dx movement))
                 (incf wy (* dy movement))))
              ((:forward :backward)
               (incf x (* movement wx (if (eq :forward action) 1 -1)))
               (incf y (* movement wy (if (eq :forward action) 1 -1))))
              ((:left :right)
               (destructuring-bind (new-wx new-wy)
                   (turn* wx wy action movement)
                 (setf wx new-wx
                       wy new-wy))))
            ;; (print (list action movement '=> x y orientation wx wy))
        :finally (return (+ (abs x) (abs y)))))
