
(defpackage #:advent2020.day07
  (:use #:cl #:alexandria #:advent2020.util #:arrows #:split-sequence)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2020.day07)



(defun kw (str)
  (make-keyword (string-upcase str)))

(defun parse-luggage (line)
  (let* ((sentence (split-sequence #\Space line))
         (color (cons (kw (first sentence))
                      (kw (second sentence)))))
    (cons color
          (loop :for (num color1 color2) :on (cddddr sentence) :by #'cddddr
                :if (string= num "no")
                  :do (return nil)
                :else
                  :collect (cons (cons (kw color1) (kw color2))
                                 (parse-integer num))))))

(defun make-luggage-table (luggages)
  (loop :with table := (make-hash-table :test 'equal)
        :for (color . items) :in luggages
        :do (setf (gethash color table) items)
        :finally (return table)))

(defparameter +input+
  (make-luggage-table (parse-lines 'parse-luggage)))

(defun bag-items (color)
  (gethash color +input+))

(defun has-shiny (bag-color target)
  (loop :for (color . quantity) :in (bag-items bag-color)
          :thereis (or (equal target color)
                       (has-shiny color target))))

(defun solve-part-1 ()
  (loop :for color :being :the :hash-key :of +input+
        :count (has-shiny color '(:shiny . :gold))))

(defun bag-capacity (color)
  (loop :for (color . quantity) :in (bag-items color)
        :sum (* (1+ (bag-capacity color))
                quantity)))

(defun solve-part-2 ()
  (bag-capacity '(:shiny . :gold)))
