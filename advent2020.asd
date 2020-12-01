;;;; advent2020.asd

(asdf:defsystem #:advent2020
  :description "Describe advent2020 here"
  :author "Samuel Hunter"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :depends-on (#:alexandria #:arrows)
  :serial t
  :components ((:file "util")))
