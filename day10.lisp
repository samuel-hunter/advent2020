
(defpackage #:advent2020.day10
  (:use #:cl #:alexandria #:advent2020.util #:org.tfeb.hax.memoize)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day10)



(defparameter +input+
  (sort (parse-lines #'parse-integer) #'<))

(defun solve-part-1 ()
  (loop :for (a b)
          :on (cons 0 +input+)
        :while b

        :count (= 1 (- b a)) :into 1-diffs
        :count (= 3 (- b a)) :into 3-diffs

        ;; 1+ on 3-diffs for implicit 3-jump laptop joltage.
        :finally (return (* 1-diffs
                            (1+ 3-diffs)))))

(defun arrangements (adapters)
  "Recursively return the number of possible arrangements of the given adapters."
  ;; There is only 1 arrangement for a single adapter.
  (when (= 1 (length adapters))
    (return-from arrangements 1))

  (loop :with start := (first adapters)
        :for (val . rest) :on (rest adapters)
        :while (<= (- val start) 3)
        :sum (arrangements (cons val rest))))

;; Memoize the function to remember partial solutions.
(memoize-function 'arrangements :test #'equal)

(defun solve-part-2 ()
  (arrangements (cons 0 +input+)))
