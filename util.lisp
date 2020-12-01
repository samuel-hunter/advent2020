
(defpackage #:advent2020.util
  (:use #:cl #:alexandria)
  (:export #:read-puzzle-text
           #:read-puzzle-sexp))

(in-package #:advent2020.util)



(defparameter +input-directory+
  (asdf:system-relative-pathname :advent2020
                                 "input/"))

(defun package->inputname (package suffix)
  "Convert a package to its associated input file name."
  (let* ((name (package-name package))
         (base-name (string-downcase (subseq name #.(length "ADVENT2020."))))
         (full-name (concatenate 'string base-name suffix)))
    full-name))

(defun normalize-name-designator (name-designator suffix)
  "Convert any name designators into a proper input file name."
  (etypecase name-designator
    (package (package->inputname name-designator suffix))
    (null (package->inputname *package* suffix))
    (keyword (concatenate 'string (string-downcase name-designator) suffix))
    (string name-designator)))

(defun read-puzzle-text (&optional name)
  "Read a file from the input directory as a text file. With no NAME,
the file is determined based on the package name."
  (setf name (normalize-name-designator name ".txt"))
  (read-file-into-string (merge-pathnames name +input-directory+)))

(defun read-puzzle-sexp (&optional name)
  "Read a file from the input directory as an s-expression. With no
NAME, the file is determined based on the package name."
  (setf name (normalize-name-designator name ".lisp"))
  (with-open-file (stream (merge-pathnames name +input-directory+))
    (read stream)))
