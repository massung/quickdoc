;;;; QuickDoc Markup for Common Lisp
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
  (:use :cl :re :lexer :csv :parse :html :url)
  (:nicknames :qd)
  (:export
   #:parse-quickdoc
   #:compile-quickdoc
   #:render-quickdoc

   ;; reader methods
   #:quickdoc-tags
   #:quickdoc-body))

;;; ----------------------------------------------------

(in-package :quickdoc)

;;; ----------------------------------------------------

(defparameter *default-css*
  #.(let ((root (or *compile-file-pathname* *load-pathname*)))
      (slurp (merge-pathnames "quickdoc.css" root))))

;;; ----------------------------------------------------

(defparameter *video-domains*
  '("www.youtube.com"
    "youtube.com"
    "youtu.be"
    "y2u.be"
    "player.vimeo.com")
  "Domain names to embed videos in iframes.")

;;; ----------------------------------------------------

(defclass quickdoc ()
  ((tags :initarg :tags :reader quickdoc-tags)
   (body :initarg :body :reader quickdoc-body))
  (:documentation "Document of parsed nodes and meta comment tags."))

;;; ----------------------------------------------------

(defun quickdoc-title (doc)
  "Return the title tag of a document (if any)."
  (let ((tag (assoc "title" (quickdoc-tags doc) :test #'equalp)))
    (if tag
        (<title> (second tag))
      (<title> "Untitled"))))

;;; ----------------------------------------------------

(defun quickdoc-meta-tags (doc)
  "Return a list of meta tags for the document."
  (flet ((meta (name &optional content)
           (if content
               (<meta> :name name :content content)
             (<meta> :name name))))
    (mapcar #'(lambda (tag) (apply #'meta tag)) (quickdoc-tags doc))))

;;; ----------------------------------------------------

(defun parse-with-lexer (p lexer string)
  "Helper function for parsing a string with a given lexer."
  (with-lexer (state lexer string)
    (with-token-reader (next-token state)
      (parse p next-token))))

;;; ----------------------------------------------------

(defun parse-elements (string)
  "Parse quickdoc elements from a string."
  (parse-with-lexer (.many 'element) 'body-lexer string))

;;; ----------------------------------------------------

(defun parse-text-spans (string)
  "Parse quickdoc text spans from a string."
  (parse-with-lexer (.many 'span) 'span-lexer string))

;;; ----------------------------------------------------

(defun parse-quickdoc (pathname)
  "Create a quickdoc object by parsing the file at the given location."
  (let ((source (slurp pathname)))
    (destructuring-bind (tags body)
        (parse-with-lexer 'quickdoc 'quickdoc-lexer source)
      (make-instance 'quickdoc :tags tags :body body))))

;;; ----------------------------------------------------

(defun compile-quickdoc (doc pathname &optional stylesheets embed)
  "Render the HTML of a quickdoc to a file."
  (with-open-file (fs pathname :direction :output :if-exists :supersede)
    (let ((html (render-quickdoc doc stylesheets embed)))
      (html-render html fs))))

;;; ----------------------------------------------------

(defun render-quickdoc (doc &optional stylesheets embed)
  "Convert the document to HTML and return it."
  (<html> (<head> (quickdoc-meta-tags doc)
                  (quickdoc-title doc)

                  ;; embed the optional styleshee
                  (if (null stylesheets)
                      (<style> *default-css*)
                    (loop
                       for css in stylesheets
                       collect (if embed
                                   (<style> (slurp css))
                                 (<link> :rel "stylesheet" :href css)))))

          ;; wrap the document in a div with a named class
          (<body> (<article> (quickdoc-body doc)))))
