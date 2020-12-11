
(defpackage #:advent2020.day11
  (:use #:cl #:alexandria #:advent2020.util #:org.tfeb.hax.memoize)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day11)



(defun parse-grid (lines)
  (loop :with grid := (make-array (list (length (first lines))
                                        (length lines))
                                  :element-type 'character
                                  :initial-element #\.)
        :for line :in lines
        :for y :upfrom 0
        :do (loop :for c :across line
                  :for x :upfrom 0
                  :do (setf (aref grid x y) c))
        :finally (return grid)))

(defparameter +input+ (parse-grid (parse-lines #'identity)))

(defun first-seat (grid origin-x origin-y dx dy)
  (loop :for x := (+ origin-x dx) :then (+ x dx)
        :for y := (+ origin-y dy) :then (+ y dy)
        :while (and (<= 0 x (1- (array-dimension grid 0)))
                    (<= 0 y (1- (array-dimension grid 1))))
        :for seat := (aref grid x y)
        :unless (char= seat #\.)
          :do (return (values seat t))
        :finally (return (values #\. nil))))

;; (defun is-occupied (grid x y)
;;   (when (or (< x 0) (< y 0)
;;             (>= x (array-dimension grid 0))
;;             (>= y (array-dimension grid 1)))
;;     (return-from is-occupied nil))
;;   (char= #\# (aref grid x y)))

(defun is-occupied (grid x y dx dy)
  (char= #\# (first-seat grid x y dx dy)))

;; (defun adjacents (grid x y)
;;   (loop :for adjacent :in (list
;;                             (is-occupied grid (1- x) (1- y))
;;                             (is-occupied grid x (1- y))
;;                             (is-occupied grid (1+ x) (1- y))

;;                             (is-occupied grid (1- x) y)
;;                             (is-occupied grid (1+ x) y)

;;                             (is-occupied grid (1- x) (1+ y))
;;                             (is-occupied grid x (1+ y))
;;                             (is-occupied grid (1+ x) (1+ y)))
;;         :count adjacent))

(defun adjacents (grid x y)
  (loop :for (dx dy) :in '((-1 -1)
                           (-1 0)
                           (-1 +1)
                           (0 -1)
                           (0 +1)
                           (+1 -1)
                           (+1 0)
                           (+1 +1))
        :count (is-occupied grid x y dx dy)))

(defun eval-grid (grid)
  (let* ((size (array-dimensions grid))
         (width (first size))
         (height (Second size))
         (target (copy-array grid)))
    (dotimes (x width)
      (dotimes (y height)
        (let ((seat (aref grid x y)))
          (cond
            ((and (char= #\L seat)
                  (= (adjacents grid x y) 0))
             (setf (aref target x y) #\#))
            ((and (char= #\# seat)
                  (>= (adjacents grid x y) 5))
             (setf (aref target x y) #\L))))))
    target))

(defun print-grid (grid)
  (loop :for y :below (array-dimension grid 1)
        :do (loop :for x :below (array-dimension grid 0)
                  :do (princ (aref grid x y)))
            (terpri))
  (terpri))

(defun solve-part-1 ()
  (loop :for start := (copy-array +input+) :then next
        :for next := (eval-grid start)
        :for iterations :upfrom 0
        :until (equalp start next)
        :do (print-grid next)
        :finally (return
                   (loop :for x :below (array-dimension next 0)
                         :sum (loop
                                :for y :below (array-dimension next 1)
                                :for seat := (aref next x y)
                                :count (char= seat #\#))))))

  (defun solve-part-2 ())
