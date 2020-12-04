
(defpackage #:advent2020.day02
  (:use #:cl #:alexandria #:advent2020.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day02)



(defparameter +scanner+ (ppcre:create-scanner "(\\d+)-(\\d+) (\\w): (\\w+)"))
(defun parse-field (line)
  ;; Bind each regex group into a variable.
  (ppcre:register-groups-bind (num1 num2 letter password)
      (+scanner+ line :sharedp t)
    (assert (and num1 num2 letter password))
    ;; Pack them back in after converting them to the proper type.
    (list (parse-integer num1)
          (parse-integer num2)
          (char letter 0)
          password)))

(defparameter +input+ (parse-lines #'parse-field))

(defun count-occurrences (char string)
  "Count the occurences of CHAR in STRING."
  (loop :for c :across string
        :count (char= c char)))

(defun solve-part-1 ()
  (loop :for (min max letter password) :in +input+
        :for occurrences := (count-occurrences letter password)
        :count (<= min occurrences max)))

(defun solve-part-2 ()
  (loop :for (pos1 pos2 letter password) :in +input+
        :count (xor (char= letter
                           (char password (1- pos1)))
                    (char= letter
                           (char password (1- pos2))))))
