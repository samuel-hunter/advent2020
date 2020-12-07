
(defpackage #:advent2020.day07
  (:use #:cl #:alexandria #:advent2020.util #:arrows #:split-sequence)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day07)



(defun make-color (str1 str2)
  "Format and intern a color combination into a keyword."
  ;; Technically bad-practice to dynamically intern symbols. from user
  ;; input, buuuut this is Advent of Code, and so I'll deliberately
  ;; violate this security concern to intern 595-ish symbols.
  (make-keyword (format nil "~A-~A"
                        (string-upcase str1)
                        (string-upcase str2))))

(defun parse-bag (line)
  "Convert a text line of a bag into a list of its color, followed by
an associative list of child bag colors and their quantities."
  (let* ((sentence (split-sequence #\Space line))
         (color (make-color (first sentence)
                            (second sentence))))
    (cons color
          ;; Iterate through every four words (with the first four
          ;; skipped), and bind the first three to (num color1 color2)
          (loop :for (num color1 color2) :on (cddddr sentence) :by #'cddddr
                ;; Some bags sometimes start with "X Y bags contain no
                ;; other bags." Return an empty list instead.
                :if (string= num "no")
                  :do (return ())
                :else
                  ;; Return a pair of the bag child's color and the
                  ;; quantity the parent holds.
                  :collect (cons (make-color color1 color2)
                                 (parse-integer num))))))

(defparameter +input+
  ;; Because each parsed line has a color as the head of the list, a
  ;; list of these lines turn into an associative list. Convert this
  ;; into a hash table.
  (alist-hash-table (parse-lines 'parse-luggage)))

(defun bag-items (color)
  (gethash color +input+))

(defun has-color (bag target-color)
  "Return whether BAG indirectly contains TARGET-COLOR."
  (loop :for (color . quantity) :in bag
          :thereis (or (equal target-color color)
                       (has-color (bag-items color) target-color))))

(defun solve-part-1 ()
  ;; Count all bags that has the shiny gold bag.
  (loop :for bag :being :the :hash-value :of +input+
        :count (has-color bag :shiny-gold)))

(defun bag-capacity (color)
  "Return the number of bags the bag colored COLOR contains."
  (loop :for (color . quantity) :in (bag-items color)
        :sum (* (1+ (bag-capacity color))
                quantity)))

(defun solve-part-2 ()
  (bag-capacity :shiny-gold))
