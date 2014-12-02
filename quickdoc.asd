(defpackage :quickdoc-asd
  (:use :cl :asdf))

(in-package :quickdoc-asd)

(defsystem :quickdoc
  :name "quickdoc"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "QuickDoc Markup for LispWorks."
  :serial t
  :components ((:file "quickdoc")
               (:file "span")
               (:file "render"))
  :depends-on ("lexer" "html"))
