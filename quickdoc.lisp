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
  (:use :cl :lw :capi :re :lexer :csv :parsergen :html)
  (:nicknames :qd)
  (:export
   #:parse-quickdoc
   #:render-quickdoc
   #:compile-quickdoc))

(in-package :quickdoc)

(defclass quickdoc ()
  ((title :initform nil :accessor quickdoc-title)
   (meta  :initform nil :accessor quickdoc-meta)
   (body  :initform nil :accessor quickdoc-body))
  (:documentation "Document object holding all parsed nodes and meta comment tags."))

(defstruct markup-node      "Top-level node." class text spans)
(defstruct text-node        "Encoded plain text." text)
(defstruct br-node          "Hard break.")
(defstruct tt-node          "Monospace text." text)
(defstruct link-node        "Link." url alt)
(defstruct strong-node      "Strong text." spans)
(defstruct em-node          "Emphasized text." spans)
(defstruct strike-node      "Strike-through text." spans)
(defstruct superscript-node "Superscript text." spans)
(defstruct subscript-node   "Subscript text." spans)
(defstruct th-node          "Table header." text)
(defstruct td-node          "Table data." text spans)
(defstruct media-node       "Images and videos." videop url caption)

(defparameter *p-class* :p
  "Default markup-node-class to use for paragraphs.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +quickdoc-css+ #.(slurp (merge-pathnames #p"quickdoc.css" *compile-file-pathname*))
    "The default CSS to use for rendered markup."))

(defun compile-quickdoc (source &rest render-args &key output-file &allow-other-keys)
  "Read a QuickDoc source file and render it as HTML to a file."
  (unless output-file
    (setf output-file (merge-pathnames (make-pathname :type "html") (pathname source))))

  ;; parse the quickdoc file
  (when-let (doc (parse-quickdoc (slurp source)))
    (prog1
        doc
      (with-open-file (stream output-file :direction :output :if-exists :supersede)
        (apply 'render-quickdoc doc :stream stream render-args)))))

(defun render-quickdoc (doc &key stream title stylesheet &allow-other-keys)
  "Return the HTML body and META tags for a document."
  (let ((html `(:html ()
                (:head ()

                 ;; set the title of the document
                 ,@(when-let (s (or title (quickdoc-title doc))) `((:title () ,s)))

                 ;; write a link or embed a stylesheet
                 ,@(when stylesheet `((:link ((:rel "stylesheet") (:href ,stylesheet) (:type "text/css")))))

                 ;; write the meta tags
                 ,@(loop for (k v) in (quickdoc-meta doc) collect `(:meta ((:name ,k) (:content ,v)))))

                ;; write the body
                (:body () ,@(mapcar 'render-node (quickdoc-body doc))))))

    ;; make sure things will display properly in IE
    (format stream "<!DOCTYPE HTML>")

    ;; output the html to the stream
    (html html stream)))

(defun parse-quickdoc (string)
  "Given a string, read each line and pass it through the node group parser."
  (let ((doc (make-instance 'quickdoc)))
    (with-input-from-string (s string)
      (loop while (char= (peek-char nil s nil #\null) #\@)
            
            ;; read the line and parse it
            for comment = (read-line s)
            for delim = (position #\: comment :test #'char=)
            
            ;; get the name and content
            for name = (string-trim '(#\space #\tab) (subseq comment 1 delim))
            for content = (string-trim '(#\space #\tab) (if (null delim)
                                                            ""
                                                          (subseq comment (1+ delim))))
            
            ;; add the comment to the meta tags for the document
            when (plusp (length name))
            do (if (string-equal name :title)
                   (setf (quickdoc-title doc) content)
                 (push (list (string-downcase name) content) (quickdoc-meta doc)))
            
            ;; done, now read the body
            finally (progn
                      (setf (quickdoc-body doc) (parse-group #'(lambda () (read-line s nil nil))))

                      ;; return the final document
                      (return doc))))))

(defun parse-node (next-line &optional recursive-p single-line-p)
  "Return a markup node given the start of a line of text."
  (when-let (line (funcall next-line))
    (macrolet ((try (pattern &body body)
                 (let ((m (gensym)))
                   `(with-re-match (,m (match-re ,(compile-re pattern) line))
                      (multiple-value-bind (class text)
                          (progn ,@body)
                        (return-from parse-node (make-markup-node :class class :text (list text))))))))

      ;; headings and breaks cannot be recursively inside anything
      (unless recursive-p
        (try "====.*"     (values :h4 (string-trim '(#\= #\space #\tab) $$)))
        (try "===.*"      (values :h3 (string-trim '(#\= #\space #\tab) $$)))
        (try "==.*"       (values :h2 (string-trim '(#\= #\space #\tab) $$)))
        (try "=.*"        (values :h1 (string-trim '(#\= #\space #\tab) $$)))
        
        ;; horizontal rule
        (try "%-%-%-.*"   (values :hr (string-trim '(#\- #\space #\tab) $$))))

      ;; horizontal rules, blockquotes, pre's, images, and tasks cannot be in lists
      (unless single-line-p
      
        ;; blockquotes
        (try ">%s(.*)"    (values :bq $1))
        (try ">%s*$"      (values :bq ""))
      
        ;; preformatted lines
        (try ":%s(.*)"    (values :pre $1))
        (try ":%s*$"      (values :pre ""))

        ;; images
        (try "!<%s(.*)"   (values :img< (string-trim '(#\space #\tab) $1)))
        (try "!>%s(.*)"   (values :img> (string-trim '(#\space #\tab) $1)))
        (try "!%s(.*)"    (values :img= (string-trim '(#\space #\tab) $1)))

        ;; tables
        (try "%|%s(.*)"   (values :table $1)))
      
      ;; unordered and ordered list items
      (try "%*%s(.*)"     (values :ul $1))
      (try "#%s(.*)"      (values :ol $1))
      
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
    
      ;; lists :dl
      ((:ul :ol)
       (let ((*p-class* :li))
         (parse-group #'(lambda () (pop text)) t t)))

      ;; multi-line tables
      ((:table)
       (parse-table node))

      ;; images and video
      ((:img= :img< :img>)
       (list (parse-media node)))

      ;; headings aren't marked up
      ((:h1 :h2 :h3 :h4)
       (mapcar #'(lambda (s) (make-text-node :text s)) text))

      ;; list items can't merge, so just render them
      ((:li)
       (parse 'span-parser 'span-lexer (first text)))
    
      ;; everything else is plain text and joined by newlines
      (otherwise
       (parse 'span-parser 'span-lexer (format nil "~{~a~^~%~}" text))))))

(defun parse-group (next-line &optional recursive-p single-line-p)
  "Given a group of lines in the same section, parse them into nodes."
  (loop with node = (parse-node next-line recursive-p single-line-p)
        while node
        
        ;; get each node, ignore empty lines
        when (markup-node-class node)
        collect node
        
        ;; merge node group text together
        do (setf (markup-node-spans node)
                 (if (find (markup-node-class node) '(:p :ol :ul :table :bq :pre))
                     (loop with tail = (markup-node-text node)
                           
                           ;; read subsequent lines from the source
                           for next = (parse-node next-line recursive-p single-line-p)
                           
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
                     (setf node (parse-node next-line recursive-p single-line-p)))))))

(defun parse-media (node)
  "Split the text into a URL and caption. Determine if the URL is to a video."
  (multiple-value-bind (url cap)
      (split-re #/%s*%|%s*/ (first (markup-node-text node)))
    (make-media-node :url url :caption cap)))

(defun parse-table (node)
  "Create headers and data cells from each row."
  (flet ((make-cell (cell) (make-td-node :text cell :spans (parse 'span-parser 'span-lexer cell)))
         (make-header (cell) (make-th-node :text cell)))
    (loop with records = (mapcar #'parse-csv (markup-node-text node))

          ;; the first record is the header
          with header = (mapcar #'make-header (first (pop records)))

          ;; every other record is a list of cells
          while records collect (mapcar #'make-cell (first (pop records))) into cells

          ;; return the header and cells
          finally (return (cons header cells)))))
