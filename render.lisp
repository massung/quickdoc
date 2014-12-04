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
      (:h1   `(:h1 () ,@child-spans))
      (:h2   `(:h2 () ,@child-spans))
      (:h3   `(:h3 () ,@child-spans))
      (:h4   `(:h4 () ,@child-spans))

      ;; paragraphs and blockquotes
      (:p    `(:p () ,@child-spans))
      (:bq   `(:blockquote () ,@child-spans))

      ;; justified images
      (:img  `(:center () ,@(multiple-value-bind (url cap)
                                (split-re #/%s*\|%s*/ (first (markup-node-text node)))
                              `((:img ((:src ,url))) ,@(when cap `((:div ((:class "caption")) ,cap)))))))
      
      ;; lists
      (:ul   `(:ul () ,@child-spans))
      (:ol   `(:ol () ,@child-spans))

      ;; list items
      (:li   `(:li () ,@child-spans))
      
      ;; horizontal rules
      (:hr   (if (plusp (length (first (markup-node-text node))))
                 `(:center ()
                   (:table ((:style "width:100%;margin:0;padding:0;margin-left:auto;margin-right:auto")
                            (:cellspacing 0)
                            (:cellpadding 0))
                    (:tr ()
                     (:td () (:hr ((:style "width:100%"))))
                     (:td ((:class "hr") (:style "width:1px;padding:0 10px;white-space:nowrap;"))
                      ,@(markup-node-text node))
                     (:td () (:hr ((:style "width:100%")))))))
               `(:hr)))

      ;; pre-formatted text
      (:pre  `(:pre () ,(format nil "~{~a~%~}" (markup-node-text node)))))))

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