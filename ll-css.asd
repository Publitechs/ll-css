(in-package :cl-user)
(defpackage ll-css-asd
  (:use :cl :asdf))
(in-package :ll-css-asd)

(defsystem ll-css
  :version "0.1"
  :author "Ilya Khaprov <mail@publitechs.com>"
  :license "MIT"
  :depends-on (:alexandria)
  :components ((:file "interface")
               (:file "css"))
  :description "Adds lisp functions and variables to css")


               
