
(defpackage #:advent2020.day02
  (:use #:cl #:alexandria #:advent2020.util #:arrows)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day02)



(defun parse-input ()
  (with-puzzle-file (stream)
    (loop :with scanner := (ppcre:create-scanner "(\\d+)-(\\d+) (\\w): (\\w+)")
          :for line := (read-line stream nil)
          :while line

          ;; Scan the substrings of the line.
          :for (num1 num2 letter password)
            := (-<>
                ;; Scan each line -- scan-to-strings returns multiple
                ;; values
                (multiple-value-list
                 (ppcre:scan-to-strings scanner line))
                ;; ...and we want the second one.
                (second <>)
                ;; Convert to list.
                (coerce <> 'list))
          ;; Pack it back into a list.
          :collect (list (parse-integer num1)
                         (parse-integer num2)
                         (char letter 0) ;; Convert letter "string" to a char.
                         password))))

(defparameter +input+ (parse-input))

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
