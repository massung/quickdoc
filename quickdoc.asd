(defpackage :quickdoc-asd
  (:use :cl :asdf))

(in-package :quickdoc-asd)

(defsystem :quickdoc
  :name "quickdoc"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "QuickDoc Markup for Common Lisp."
  :serial t
  :components ((:file "quickdoc")
               (:file "lexer")
               (:file "parser"))
  :depends-on ("base64" "csv" "re" "parse" "lexer" "html" "url"))
