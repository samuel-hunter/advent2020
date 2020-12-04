
(defpackage #:advent2020.day04
  (:use #:cl #:alexandria #:advent2020.util #:arrows #:split-sequence)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day04)



(defun parse-passports ()
  (with-puzzle-file (stream)
    (loop :with passport := ()
          :with passports := ()

          :for line := (read-line stream nil)
          :while line

          :if (string= line "")
            :do (push passport passports)
                (setf passport ())
          :else
            :do (loop :for cred :in (split-sequence #\Space line)
                      :for (key value) := (split-sequence #\: cred)
                      :do (push value passport)
                          (push (make-keyword (string-upcase key)) passport))
          :finally (push passport passports)
                   (return passports))))

(defparameter +input+ (parse-passports))

(defun solve-part-1 ()
  (loop :for passport :in +input+
        :for attribs := (/ (length passport) 2)
        :count (or (= attribs 8)
                   (and (= attribs 7)
                        (not (member :cid passport))))))

(defun parse-height (heightstr)
  (if (ends-with-subseq "cm" heightstr)
      (cons (parse-integer heightstr :junk-allowed t) :cm)
      (cons (parse-integer heightstr :junk-allowed t) :in)))

(defun valid-color-p (colorstr)
  (and (eq (char colorstr 0) #\#)
       (= 7 (length colorstr))
       (loop :for c :across (subseq colorstr 1)
             :always (member (char-downcase c) '(#\a #\b #\c #\d #\e #\f #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))))

(defun valid-p (passport)
  (let ((attribs (/ (length passport) 2)))
    (unless (or (= attribs 8)
              (and (= attribs 7)
                   (not (member :cid passport))))
      (return-from valid-p nil)))
  (let* (
         (byr (parse-integer (getf passport :byr)))
         (iyr (parse-integer (getf passport :iyr)))
         (eyr (parse-integer (getf passport :eyr)))
         (hgt (parse-height (getf passport :hgt)))
         (hcl (getf passport :hcl))
         (ecl (getf passport :ecl))
         (pid (getf passport :pid)))
    (and
         (<= 1920 byr 2002)
         (<= 2010 iyr 2020)
         (<= 2020 eyr 2030)
         (if (eq (cdr hgt) :cm)
             (<= 150 (car hgt) 193)
             (<= 59 (car hgt) 76))
         (valid-color-p hcl)
         (member ecl '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=)
         (= 9 (length pid)))))

(defun solve-part-2 ()
  (loop :for passport :in +input+
        :count (valid-p passport)))
