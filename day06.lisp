
(defpackage #:advent2020.day06
  (:use #:cl #:alexandria #:advent2020.util #:arrows)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day06)



(defun question-number (char)
  (- (char-code char) #.(char-code #\a)))

(defun parse-answers (lines)
  (loop :for line :in lines
        ;; Construct a 26-bit bit array for each line of questions.
        :collect (loop :with bit-array := (make-array 26 :element-type 'bit)
                       :for question :across line
                       :do (setf (bit bit-array (question-number question)) 1)
                       :finally (return bit-array))))

(defun count-answers (answer-map)
  (loop :for answer-bit :below 26
        :count (= 1 (bit answer-map answer-bit))))

(defparameter +input+ (parse-forms 'parse-answers))

(defun fold-any-answers (group)
  "Reduce a group to a bit-array of answers affirmed by ANY group member."
  (reduce #'bit-ior group))

(defun solve-part-1 ()
  (->>
   ;; Take each group and fold them up...
   (mapcar 'fold-any-answers +input+)
   ;; Count the answers for each group...
   (mapcar 'count-answers)
   ;; ...and sum it up!
   (reduce #'+)))

(defun fold-all-answers (group)
  "Reduce a group to a bit-array of answers affirmed by ALL group members."
  (reduce #'bit-and group))

(defun solve-part-2 ()
  (->>
   ;; Take each group and fold them up...
   (mapcar 'fold-all-answers +input+)
   ;; Count the answers for each group...
   (mapcar 'count-answers)
   ;; ...and sum it up!
   (reduce #'+)))
