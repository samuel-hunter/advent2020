
(defpackage #:advent2020.day02
  (:use #:cl #:alexandria #:advent2020.util #:split-sequence)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day02)



(defun parse-input (input)
  (loop :for line :in (split-sequence #\Newline input)
        :until (string= line "")
        :for (min max letter password)
          := (coerce (second (multiple-value-list (ppcre:scan-to-strings "(\\d+)-(\\d+) (\\w): (\\w+)"
                                                                                                      line)))
                                                  'list)
        :collect (list :min (parse-integer min)
                       :max (parse-integer max)
                       :letter (char letter 0)
                       :password password)))

(defparameter +input+ (parse-input (read-puzzle-text)))

(defun count-occurrences (char string)
  (loop :for i :below (length string)
        :for c := (char string i)
        :counting (char= c char)))

(defun solve-part-1 ()
  (loop :for pass :in +input+
        :do (print pass)
        :counting (<= (getf pass :min)
                      (count-occurrences (getf pass :letter)
                                         (getf pass :password))
                      (getf pass :max))))

(defun solve-part-2 ()
  (loop :for pass :in +input+
        :do (print pass)
        :counting (xor (char= (getf pass :letter)
                              (char (getf pass :password) (1- (getf pass :min))))
                       (char= (getf pass :letter)
                              (char (getf pass :password) (1- (getf pass :max)))))))
