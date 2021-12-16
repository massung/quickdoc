(asdf:defsystem :quickdoc
  :name "quickdoc"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "QuickDoc Markup for Common Lisp."
  :serial t
  :components ((:file "quickdoc")
               (:file "lexer")
               (:file "parser"))
  :depends-on ("boost-base64"
               "boost-csv"
               "boost-re"
               "boost-parse"
               "boost-lexer"
               "boost-html"
               "boost-url"))
