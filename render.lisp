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

(in-package :quickdoc)

(defmethod render-node ((node markup-node))
  "Render a top-level markup node."
  (symbol-macrolet ((child-spans (mapcar #'render-node (markup-node-spans node))))
    (case (markup-node-class node)
      
      ;; headings
      (:h1   `(:div ((:class "section")) (:h1 () ,@child-spans)))
      (:h2   `(:div ((:class "section")) (:h2 () ,@child-spans)))
      (:h3   `(:div ((:class "section")) (:h3 () ,@child-spans)))

      ;; paragraphs and blockquotes
      (:p    `(:div ((:class "section")) (:p () ,@child-spans)))
      (:bq   `(:div ((:class "section")) (:blockquote () ,@child-spans)))

      ;; justified images
      (:img  `(:center ((:class "section"))
               ,@(multiple-value-bind (url caption)
                     (split-re #/\|/ (first (markup-node-text node)))
                   `((:img ((:src ,(string-trim '(#\space #\tab) url))))
                     ,@(when caption
                         `((:div ((:class "caption")) ,(string-trim '(#\space #\tab) caption))))))))
      
      ;; lists
      (:ul   `(:div ((:class "section")) (:ul () ,@child-spans)))
      (:ol   `(:div ((:class "section")) (:ol () ,@child-spans)))

      ;; list items
      (:li   `(:li () ,@child-spans))
      
      ;; horizontal rules
      (:hr   `(:div ((:class "section")) ,(if (plusp (length (first (markup-node-text node))))
                                              `(:table ((:class "hr"))
                                                (:tr ()
                                                 (:td () (:hr))
                                                 (:td ((:class "hr")) ,@(markup-node-text node))
                                                 (:td () (:hr))))
                                            `(:hr))))

      ;; pre-formatted text
      (:pre  `(:div ((:class "section")) (:pre () ,(format nil "~{~a~%~}" (markup-node-text node))))))))

(defmethod render-node ((node text-node))
  "Render a simple text node."
  (text-node-text node))

(defmethod render-node ((node tt-node))
  "Render a monospace text node."
  `(:tt () ,(tt-node-text node)))

(defmethod render-node ((node link-node))
  "Render a link node."
  `(:a ((:href ,(link-node-url node))) ,(link-node-alt node)))

(defmethod render-node ((node strong-node))
  "Render strongly emphasized text."
  `(:strong () ,@(mapcar #'render-node (strong-node-spans node))))

(defmethod render-node ((node em-node))
  "Render emphasized text."
  `(:em () ,@(mapcar #'render-node (em-node-spans node))))

(defmethod render-node ((node superscript-node))
  "Render superscript text."
  `(:sup () ,@(mapcar #'render-node (superscript-node-spans node))))

(defmethod render-node ((node subscript-node))
  "Render subscript text."
  `(:sub () ,@(mapcar #'render-node (subscript-node-spans node))))