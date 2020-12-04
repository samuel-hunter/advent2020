
(defpackage #:advent2020.day04
  (:use #:cl #:alexandria #:advent2020.util #:arrows #:split-sequence)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day04)



(defun parse-height (height-str)
  "Parse the HEIGHT-STR into a CONS cell, with its CAR being the
height integer, and its CDR being either :IN or :CM."
  (cons (parse-integer height-str :junk-allowed t)
        (if (ends-with-subseq "cm" height-str)
            :cm :in)))

(defstruct passport
  byr iyr eyr hgt hcl ecl pid cid)

(defparameter +required-fields+
  ;; Everything except the Country ID (cid)
  '(byr iyr eyr hgt hcl ecl pid))

(defun passport-has-required-fields (passport)
  (loop :for field :in +required-fields+
        ;; Count the number of non-nil slots
        :count (slot-value passport field) :into fields
        ;; If the numer of slots matches the number of required
        ;; fields, then it's OK!
        :finally (return (= fields (length +required-fields+)))))

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

(defun parse-passports ()
  (with-puzzle-file (stream)
    (loop :with kwargs := () ;; keyword args for the passport

          :for line := (read-line stream nil)
          :while line

          ;; An extra line separates passports.
          :if (string= line "")
            ;; Pass forward the kwargs to #'make-passport and collect them.
            :collect (apply 'make-passport* kwargs) :into passports
            :and :do (setf kwargs ())
          :else
            ;; Go through each key:value pair and add them to the keyword args list.
            :do (loop :for cred :in (split-sequence #\Space line)
                      :for (key value) := (split-sequence #\: cred)
                      ;; Prepend the value, and then the keyword, into the linked list.
                      :do (push value kwargs)
                          (push (make-keyword (string-upcase key)) kwargs))
          :finally
             (return (cons (apply 'make-passport* kwargs)
                           passports)))))

(defparameter +input+ (parse-passports))

(defun solve-part-1 ()
  (loop :for passport :in +input+
        :count (passport-has-required-fields passport)))

(defun hex-char-p (char)
  (let ((code (char-code char)))
    (or (<= (char-code #\a) code (char-code #\f))
        (<= (char-code #\0) code (char-code #\9)))))

(defun valid-color-p (colorstr)
  (and (eq (char colorstr 0) #\#)
       (= 7 (length colorstr))
       (loop :for c :across (subseq colorstr 1)
             :always (hex-char-p c))))

(defun passport-valid-p (passport)
  (and (passport-has-required-fields passport)
       (<= 1920 (passport-byr passport) 2002)
       (<= 2010 (passport-iyr passport) 2020)
       (<= 2020 (passport-eyr passport) 2030)
       (if (eq (cdr (passport-hgt passport)) :cm)
           (<= 150 (car (passport-hgt passport)) 193)
           (<= 59 (car (passport-hgt passport)) 76))
       (valid-color-p (passport-hcl passport))
       (member (passport-ecl passport)
               '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=)
       (= 9 (length (passport-pid passport)))))

(defun solve-part-2 ()
  (loop :for passport :in +input+
        :count (passport-valid-p passport)))
