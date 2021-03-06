
(defpackage #:advent2020.day04
  (:use #:cl #:alexandria #:advent2020.util #:arrows #:split-sequence)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day04)



(defstruct passport
  byr iyr eyr hgt hcl ecl pid cid)

(defun passport-has-required-fields (passport)
  (loop :with required-fields := '(byr iyr eyr hgt hcl ecl pid)
        :for field :in required-fields

        ;; Every required field should be non-nil.
        :always (slot-value passport field)))

(defun height-value (passport)
  (car (passport-hgt passport)))

(defun height-unit (passport)
  (cdr (passport-hgt passport)))

(defun parse-height (height-str)
  "Parse the HEIGHT-STR into a CONS cell, with its CAR being the
height integer, and its CDR being either :IN or :CM."
  (cons (parse-integer height-str :junk-allowed t)
        (if (ends-with-subseq "cm" height-str)
            :cm :in)))

(defun make-passport* (&key byr iyr eyr hgt hcl ecl pid cid)
  "Create a passport instance, parsing each string keyword argument."
  (make-passport
   :byr (when byr (parse-integer byr))
   :iyr (when iyr (parse-integer iyr))
   :eyr (when eyr (parse-integer eyr))
   :hgt (when hgt (parse-height hgt))
   :hcl hcl
   :ecl ecl
   :pid pid
   :cid cid))

(defun parse-passport (lines)
  (loop :with kwargs := ()
        :for line :in lines
        :do (loop :for credential :in (split-sequence #\Space line)
                  :for (key value) := (split-sequence #\: credential)
                  ;; Prepent the key-value pair into the list of
                  ;; keyword args, value first because prepending
                  ;; happens in reverse.
                  :do (push value kwargs)
                      (push (make-keyword (string-upcase key)) kwargs))
        :finally (return (apply 'make-passport* kwargs))))

(defparameter +input+ (parse-forms #'parse-passport))

(defun solve-part-1 ()
  (loop :for passport :in +input+
        :count (passport-has-required-fields passport)))

(defun hexadecimal-p (string)
  "Return whether STRING is a valid hexadecimal number"
  (multiple-value-bind (value advance)
      (parse-integer string :junk-allowed t :radix 16)
    (declare (ignore value))
    ;; If the number of characters parsed is the same as the length of
    ;; the string, then the entire string is a hex number.
    (= advance (length string))))

(defun valid-color-p (colorstr)
  (and (= 7 (length colorstr))
       ;; Starts with a pound #
       (eq (char colorstr 0) #\#)
       ;; ...followed by 6 hex digits.
       (hexadecimal-p (subseq colorstr 1))))

(defun solve-part-2 ()
  (loop :for passport :in +input+
        :count (and (passport-has-required-fields passport)
                    (<= 1920 (passport-byr passport) 2002)
                    (<= 2010 (passport-iyr passport) 2020)
                    (<= 2020 (passport-eyr passport) 2030)
                    (if (eq (height-unit passport) :cm)
                        (<= 150 (height-value passport) 193)
                        (<= 59 (height-value passport) 76))
                    (valid-color-p (passport-hcl passport))
                    (member (passport-ecl passport)
                            '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")
                            :test #'string=)
                    (= 9 (length (passport-pid passport))))))
