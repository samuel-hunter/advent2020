
(defpackage #:advent2020.day01
  (:use #:cl #:alexandria #:advent2020.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day01)



(defparameter +input+ (read-puzzle-sexp))

(defun solve-part-1 ()
  (loop :for nums :on +input+
        :for x := (first nums)
        :do (loop :for y :in (rest nums)
                  :when (= 2020 (+ x y))
                    :do (return-from solve-part-1
                          (values (* x y) x y)))))

(defun solve-part-2 ()
  (loop :for nums :on +input+
        :for x := (first nums)
        :do (loop :for nums2 :on (rest nums)
                  :for y := (first nums2)
                  :do (loop :for z :in (rest nums2)
                            :when (= 2020 (+ x y z))
                              :do (return-from solve-part-2
                                    (* x y z))))))
