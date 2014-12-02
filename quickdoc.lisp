;;;; QuickDoc Markup for LispWorks
;;;;
;;;; Copyright (c) 2014 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :quickdoc
  (:use :cl :lw :capi :re :lexer :parsergen :html)
  (:nicknames :qd)
  (:export
   #:parse-quickdoc
   #:render-quickdoc))

(in-package :quickdoc)

(defstruct markup-node "Top-level node." class text spans)
(defstruct text-node   "Encoded plain text." text)
(defstruct tt-node     "Monospace text." text)
(defstruct link-node   "Link." url alt)
(defstruct strong-node "Strong text." spans)
(defstruct em-node     "Emphasized text." spans)

(defparameter *p-class* :p
  "Default markup-node-class to use for paragraphs.")

(eval-when (:compile-toplevel :execute)
  (defparameter *default-css* (slurp (merge-pathnames #p"default.css" *compile-file-pathname*))
    "The default CSS to use for rendered markup."))

(defun render-quickdoc (nodes &key (css-theme *default-css*) (title "Untitled QuickDoc"))
  "Return a list of HTML objects for a list of nodes."
  (html `(:html ()
          (:head ()
           (:title () ,title)
           (:style () ,css-theme))
          (:body () ,@(mapcar 'render-node nodes)))))

(defun parse-quickdoc (string)
  "Given a string, read each line and pass it through the node group parser."
  (with-input-from-string (s string)
    (parse-group #'(lambda () (read-line s nil nil)))))

(defun parse-node (line &optional recursive-p)
  "Return a markup node given the start of a line of text."
  (when line
    (macrolet ((try (pattern &body body)
                 (let ((m (gensym)))
                   `(with-re-match (,m (match-re ,(compile-re pattern) line))
                      (multiple-value-bind (class text)
                          (progn ,@body)
                        (return-from parse-node (make-markup-node :class class :text (list text))))))))

      ;; recursive elements cannot contain headings, rules, pre, or blockquotes
      (unless recursive-p

        ;; headings
        (try "===.*"      (values :h3 (string-trim '(#\= #\space #\tab) $$)))
        (try "==.*"       (values :h2 (string-trim '(#\= #\space #\tab) $$)))
        (try "=.*"        (values :h1 (string-trim '(#\= #\space #\tab) $$)))
        
        ;; horizontal rule
        (try "%-%-%-.*"   (values :hr (string-trim '(#\- #\space #\tab) $$)))
      
        ;; blockquotes
        (try ">%s+(.*)"   (values :bq $1))
      
        ;; preformatted lines
        (try ":%s?(.*)"   (values :pre $1)))

      ;; images
      (try "!%s+(.*)"     (values :img $1))
      
      ;; unordered and ordered list items
      (try "%*%s+(.*)"    (values :ul $1))
      (try "#%s+(.*)"     (values :ol $1))
      
      ;; line breaks are paragraph delimiters
      (try "%s*$"))
    
    ;; default to a paragraph
    (make-markup-node :class *p-class* :text (list line))))

(defun parse-spans (node)
  "Take the text nodes and parse them into span nodes."
  (let ((text (markup-node-text node))
        (class (markup-node-class node)))
    (case class
      
      ;; blockquotes
      ((:bq)
       (let ((*p-class* :p))
         (parse-group #'(lambda () (pop text)) t)))
    
      ;; lists
      ((:ul :ol)
       (let ((*p-class* :li))
         (parse-group #'(lambda () (pop text)) t)))
    
      ;; paragraphs, captions, list items, and headings
      ((:p :li :h1 :h2 :h3)
       (let ((para (format nil "~{~a~^ ~}" text)))
         (parse 'span-parser (tokenize 'span-lexer para)))))))

(defun parse-group (next-line &optional recursive-p)
  "Given a group of lines in the same section, parse them into nodes."
  (loop with node = (parse-node (funcall next-line) recursive-p)
        while node
        
        ;; get each node, ignore empty lines
        when (markup-node-class node)
        collect node
        
        ;; merge node group text together
        do (setf (markup-node-spans node)
                 (if (find (markup-node-class node) '(:p :ol :ul :bq :pre))
                     (loop with tail = (markup-node-text node)
                           
                           ;; read subsequent lines from the source
                           for next = (parse-node (funcall next-line) recursive-p)
                           
                           ;; merge if the classes are the same
                           while (and next (eq (markup-node-class next)
                                               (markup-node-class node)))
                           
                           ;; append the texts together
                           do (setf tail (cdr (rplacd tail (markup-node-text next))))
                           
                           ;; finalize the node and advance
                           finally (return (prog1
                                               (parse-spans node)
                                             (setf node next))))
                   (prog1
                       (parse-spans node)
                     (setf node (parse-node (funcall next-line) recursive-p)))))))