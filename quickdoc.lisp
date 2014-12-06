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
   #:render-quickdoc
   #:compile-quickdoc))

(in-package :quickdoc)

(defclass quickdoc ()
  ((title :initform nil :accessor quickdoc-title)
   (style :initform nil :accessor quickdoc-style)
   (head  :initform nil :accessor quickdoc-head)
   (body  :initform nil :accessor quickdoc-body))
  (:documentation "Document object holding all parsed nodes and meta comment tags."))

(defstruct markup-node      "Top-level node." class text spans)
(defstruct text-node        "Encoded plain text." text)
(defstruct br-node          "Hard break.")
(defstruct tt-node          "Monospace text." text)
(defstruct link-node        "Link." url alt)
(defstruct strong-node      "Strong text." spans)
(defstruct em-node          "Emphasized text." spans)
(defstruct superscript-node "Superscript text." spans)
(defstruct subscript-node   "Subscript text." spans)
(defstruct th-node          "Table header." text align)
(defstruct td-node          "Table data." text spans align)

(defparameter *p-class* :p
  "Default markup-node-class to use for paragraphs.")

(eval-when (:compile-toplevel :execute)
  (defconstant +default-css+ #.(slurp (merge-pathnames #p"default.css" *compile-file-pathname*))
    "The default CSS to use for rendered markup."))

(defun compile-quickdoc (source &optional target)
  "Read a QuickDoc source file and render it as HTML to a target file."
  (unless target
    (setf target (merge-pathnames (make-pathname :type "html") (pathname source))))

  ;; open the target file, parse the quickdoc, and render it
  (when-let (doc (parse-quickdoc (slurp source)))
    (prog1 doc
      (with-open-file (s target :direction :output :if-exists :supersede)
        (render-quickdoc doc s)))))

(defun render-quickdoc (doc &optional stream)
  "Return a list of HTML objects for a list of nodes."
  (let* ((title (or (quickdoc-title doc) "Untitled QuickDoc"))
         (html `(:html ()
                 (:head ()
                  (:meta ((:http-equiv "Content-Type") (:content "text/html") (:charset "UTF-8")))
                  
                  ;; set the title and embed the stylesheet
                  (:title () ,title)
                  
                  ;; link to the stylesheet to use if present or embed the default
                  ,(if-let (ss (quickdoc-style doc))
                       `(:link ((:rel "stylesheet") (:href ,ss) (:type "text/css")))
                     `(:style () ,+default-css+))
                  
                  ;; add all the meta information to the document
                  ,@(loop for (key value) in (quickdoc-head doc)
                          collect `(:meta ((:name ,key) (:content ,value)))))
                 (:body () ,@(mapcar 'render-node (quickdoc-body doc))))))
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
            do (cond ((string-equal name :title)      (setf (quickdoc-title doc) content))
                     ((string-equal name :stylesheet) (setf (quickdoc-style doc) content))
                     
                     ;; everything else is a meta tag
                     (t (push (list (string-downcase name) content) (quickdoc-head doc))))
            
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
        (try "!%s(.*)"    (values :img (string-trim '(#\space #\tab) $1)))

        ;; tables
        (try "|(.*)"      (values :table $$)))
      
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

      ;; headings aren't marked up
      ((:h1 :h2 :h3 :h4)
       (mapcar #'(lambda (s) (make-text-node :text s)) text))

      ;; list items can't merge, so just render them
      ((:li)
       (parse 'span-parser (tokenize 'span-lexer (first text))))
    
      ;; everything else is plain text and joined by newlines
      (otherwise
       (let ((para (format nil "~{~a~^~%~}" text)))
         (parse 'span-parser (tokenize 'span-lexer para)))))))

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

(defun parse-table (node)
  "Create headers and data cells from each row."
  (flet ((make-cell (s)
           (let* ((cell (if (char= #\= (char s 0))
                            (make-th-node :text (subseq s 1))
                          (make-td-node :text s :spans (parse 'span-parser (tokenize 'span-lexer s)))))

                  ;; get the text for this cell
                  (text (slot-value cell 'text))

                  ;; last character index
                  (n (1- (length text)))

                  ;; is there whitespace on the left and/or right?
                  (lsp (and (plusp (length text)) (whitespace-char-p (char text 0))))
                  (rsp (and (plusp (length text)) (whitespace-char-p (char text n))))

                  ;; determine the alignment of the cell
                  (align (cond ((and (not lsp) rsp) :left)
                               ((and (not rsp) lsp) :right)
                               (t                   :center))))

             ;; set the alignment and return the cell
             (prog1 cell (setf (slot-value cell 'align) align)))))

    ;; loop over all the rows, split into cells, and create
    (loop for row in (markup-node-text node)

          ;; split the row into cells, ignore empty ones
          for cells = (split-sequence "|" row :coalesce-separators t)

          ;; make a cell for each one
          collect (mapcar #'make-cell cells))))

