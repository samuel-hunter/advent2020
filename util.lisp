
(defpackage #:advent2020.util
  (:use #:cl #:alexandria #:arrows)
  (:export #:read-puzzle-sexp
           #:with-puzzle-file
           #:parse-lines
           #:parse-forms
           #:read-puzzle-grid))

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

(defun parse-lines (parser &optional name (prefix ".txt"))
  "Read each line from the puzzle text, pass it to PARSER, and collect."
  (with-puzzle-file (stream name prefix)
    (loop :for line := (read-line stream nil)
          :while line
          :collect (funcall parser line))))

(defun parse-forms (parser &optional name (prefix ".txt"))
  "Read each multiline form from the puzzle text, pass the list of
lines to PARSER, and collect."
  (with-puzzle-file (stream name prefix)
    (loop :with form := ()
          :for line := (read-line stream nil)
          :while line
          :if (string= "" line)
            :collect (funcall parser (reverse form)) :into forms
            :and :do (setf form ())
          :else
            :do (push line form)
          :finally (return (if form
                               (cons (funcall parser (reverse form))
                                     forms)
                               forms)))))

(defun read-puzzle-grid (&optional name (prefix ".txt"))
  (loop :with lines := (parse-lines #'identity name prefix)
        :with width := (length (first lines))
        :with height := (length lines)
        :with grid := (make-array (list width height)
                                  :element-type 'character)
        :for line :in lines
        :for y :upfrom 0
        :do (loop :for char :across line
                  :for x :upfrom 0
                  :do (setf (aref grid x y) char))
        :finally (return grid)))
