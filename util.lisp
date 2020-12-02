
(defpackage #:advent2020.util
  (:use #:cl #:alexandria #:arrows)
  (:export #:read-puzzle-text
           #:read-puzzle-sexp
           #:with-puzzle-file))

(in-package #:advent2020.util)



(defparameter +input-directory+
  (asdf:system-relative-pathname :advent2020
                                 "input/"))

(defun package->inputname (package suffix)
  "Convert a package to its associated input file name."
  (-<>
   (package-name package) ;; Find the package name...
   (subseq <> #.(length "ADVENT2020.")) ;; Slice off the beginning...
   (string-downcase <>) ;; Convert to lowercase...
   (concatenate 'string <> suffix) ;; And then finally add the suffix!
   ))

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

(defmacro with-puzzle-file ((stream &optional name (prefix ".txt")) &body body)
  `(with-open-file (,stream (merge-pathnames (normalize-name-designator ,name ,prefix)
                                             +input-directory+))
     ,@body))
