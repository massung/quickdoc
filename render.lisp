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
      (:h1    `(:h1 ((:style "clear:both")) ,@child-spans))
      (:h2    `(:h2 ((:style "clear:both")) ,@child-spans))
      (:h3    `(:h3 ((:style "clear:both")) ,@child-spans))
      (:h4    `(:h4 ((:style "clear:both")) ,@child-spans))

      ;; paragraphs and blockquotes
      (:p     `(:p () ,@child-spans))
      (:bq    `(:blockquote ((:style "clear:both")) ,@child-spans))

      ;; centered image
      (:img=  `(:div ((:style "clear:both"))
                ,(render-node (first (markup-node-spans node)))))

      ;; left-justified image
      (:img<  `(:div ((:class "left") (:style "float:left"))
                ,(render-node (first (markup-node-spans node)))))

      ;; right-justified image
      (:img>  `(:div ((:class "right") (:style "float:right"))
                ,(render-node (first (markup-node-spans node)))))

      ;; task definition list
      (:table `(:table ((:style "clear:both") (:rules "all") (:frame "box"))
                (:tbody ()
                 ,@(mapcar #'(lambda (r) `(:tr () ,@(mapcar #'render-node r))) (markup-node-spans node)))))
      
      ;; lists
      (:ul    `(:ul ((:style "clear:both")) ,@child-spans))
      (:ol    `(:ol ((:style "clear:both")) ,@child-spans))

      ;; list items
      (:li    `(:li () ,@child-spans))
      
      ;; horizontal rules
      (:hr    `(:div ((:class "hr") ("style" "clear:both"))
                ,(if (plusp (length (first (markup-node-text node))))
                     `(:table ((:style "width:100%;margin:0;padding:0;border:0")
                               (:cellspacing 0)
                               (:cellpadding 0))
                       (:tr ()
                        (:td () (:hr ((:style "width:100%"))))
                        (:td ((:class "hr") (:style "width:1px;padding:0 10px;white-space:nowrap;"))
                         ,@(markup-node-text node))
                        (:td () (:hr ((:style "width:100%"))))))
                   `(:hr))))

      ;; pre-formatted text
      (:pre   `(:pre ((:style "clear:both")) ,(format nil "~{~a~%~}" (markup-node-text node)))))))

(defmethod render-node ((node text-node))
  "Render a simple text node."
  (text-node-text node))

(defmethod render-node ((node br-node))
  "Render a hard break node."
  '(:br))

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

(defmethod render-node ((node strike-node))
  "Render strike-through text."
  `(:s () ,@(mapcar #'render-node (strike-node-spans node))))

(defmethod render-node ((node superscript-node))
  "Render superscript text."
  `(:sup () ,@(mapcar #'render-node (superscript-node-spans node))))

(defmethod render-node ((node subscript-node))
  "Render subscript text."
  `(:sub () ,@(mapcar #'render-node (subscript-node-spans node))))

(defmethod render-node ((node th-node))
  "Render a definition list node."
  `(:th ((:valign "top") (:align "left")) ,(th-node-text node)))

(defmethod render-node ((node td-node))
  "Render a table cell node."
  `(:td ((:valign "top") (:align "left")) ,@(mapcar #'render-node (td-node-spans node))))

(defmethod render-node ((node media-node))
  "Render an image or video."
  (let ((url (media-node-url node))
        (cap (media-node-caption node)))
    `(:center ()

      ;; determine if the what's being linked is a video or image
      ,(if (or (search "vimeo.com" url)
               (search "youtube.com" url))
           `(:iframe ((:class "video-player") (:src ,url) (:frameborder "0")))
         `(:img ((:src ,url))))

      ;; add the caption if there is one
      ,@(when cap `((:div ((:class "caption")) ,cap))))))