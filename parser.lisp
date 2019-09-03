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

(in-package :quickdoc)

;;; ----------------------------------------------------

(define-parser quickdoc
  (.all 'tags 'body))

;;; ----------------------------------------------------

(define-parser tags
  (.many (.is :tag)))

;;; ----------------------------------------------------

(define-parser body
  (.do (.is :body)
       (.many 'element)))

;;; ----------------------------------------------------

(define-parser element
  (.or 'header
       'rule
       'blockquote
       'pre
       'table
       'unordered-list
       'ordered-list
       'img
       'div
       'p))

;;; ----------------------------------------------------

(define-parser header
  (.or (.let (h (.is :h1)) (.ret (<h1> h)))
       (.let (h (.is :h2)) (.ret (<h2> h)))
       (.let (h (.is :h3)) (.ret (<h3> h)))
       (.let (h (.is :h4)) (.ret (<h4> h)))
       (.let (h (.is :h5)) (.ret (<h5> h)))
       (.let (h (.is :h6)) (.ret (<h6> h)))))

;;; ----------------------------------------------------

(define-parser rule
  (.let (caption (.is :hr))
    (.ret (<center> :style "clear:both"
                    (<table> :class "hr"
                             :cellspacing 0
                             :cellpadding 0

                             ;; allow for a caption
                             (if caption
                                 (<tr> (<td> (<hr>))
                                       (<td> :class "title" caption)
                                       (<td> (<hr>)))
                               (<tr> (<td> (<hr>)))))))))

;;; ----------------------------------------------------

(define-parser blockquote
  (.let (ss (.many1 (.is :quote)))
    (.ret (<blockquote> (parse-elements (format nil "~{~a~%~}" ss))))))

;;; ----------------------------------------------------

(define-parser pre
  (.let (ss (.many1 (.is :pre)))
    (.ret (<pre> (loop for s in ss collect s collect (<br>))))))

;;; ----------------------------------------------------

(define-parser table
  (flet ((th (value)
           (<th> :align "left" :valign "top" value))
         (td (value)
           (<td> :align "left" :valign "top" value)))
    (.let (xs (.many1 (.is :tr)))
      (.ret (destructuring-bind (th &rest trs)
                xs
              (<table> (<tr> (mapcar #'th th))
                     (mapcar #'(lambda (tr)
                                 (<tr> (mapcar #'td tr))) trs)))))))

;;; ----------------------------------------------------

(define-parser unordered-list
  (.let (ss (.many1 (.is :ul)))
    (.ret (with-lexer (lexer 'body-lexer (format nil "~{~a~%~}" ss))
            (with-token-reader (next-token lexer)
              (<ul> (parse 'list-items next-token)))))))

;;; ----------------------------------------------------

(define-parser ordered-list
  (.let (ss (.many1 (.is :ol)))
    (.ret (with-lexer (lexer 'body-lexer (format nil "~{~a~%~}" ss))
            (with-token-reader (next-token lexer)
              (<ol> (parse 'list-items next-token)))))))

;;; ----------------------------------------------------

(define-parser list-items
  (.many1 (.or 'unordered-list 'ordered-list 'list-item)))

;;; ----------------------------------------------------

(define-parser list-item
  (.let (s (.is :p))
    (.ret (<li> s))))

;;; ----------------------------------------------------

(define-parser img
  (labels ((video-p (src)
             (with-url (url src)
               (let ((host (url-domain url)))
                 (member host *video-domains* :test #'equalp))))
           (make-tag (style src &optional caption)
             (<center> :style style
                       (if (video-p src)
                           (<iframe> :src src
                                     :frameborder 0
                                     :allowfullscreen nil
                                     :width "60%"
                                     :height 300)
                         (<img> :src src))

                       ;; optional caption
                       (when caption
                         (<div> :class "caption" caption)))))
    (.or (.let (link (.is :img=))
           (.ret (apply #'make-tag "clear:both" link)))
         (.let (link (.is :img<))
           (.ret (apply #'make-tag "float:left;padding-right:8px" link)))
         (.let (link (.is :img>))
           (.ret (apply #'make-tag "float:right;padding-left:8px" link))))))

;;; ----------------------------------------------------

(define-parser div
  (.is :div))

;;; ----------------------------------------------------

(define-parser p
  (.let (ps (.many1 (.is :p)))
    (.ret (<p> (mapcar #'parse-text-spans ps)))))

;;; ----------------------------------------------------

(define-parser span
  (.or 'br
       'text
       'tt
       'link
       'strong
       'em
       'strike
       'subscript
       'superscript))

;;; ----------------------------------------------------

(define-parser br
  (.do (.is :br)
       (.ret (<br>))))

;;; ----------------------------------------------------

(define-parser text
  (.let (s (.is :text))
    (.ret s)))

;;; ----------------------------------------------------

(define-parser tt
  (.let (ss (.between (.is :tt)
                      (.is :-tt)
                      (.many (.is :text))))
    (.ret (<tt> ss))))

;;; ----------------------------------------------------

(define-parser link
  (.let (link (.is :link))
    (.ret (apply #'<a> :href link))))

;;; ----------------------------------------------------

(define-parser strong
  (.let (ss (.between (.is :strong)
                      (.is :strong)
                      (.many (.or 'br
                                  'text
                                  'tt
                                  'link
                                  'em
                                  'strike
                                  'subscript
                                  'superscript))))
    (.ret (<strong> ss))))

;;; ----------------------------------------------------

(define-parser em
  (.let (ss (.between (.is :em)
                      (.is :em)
                      (.many (.or 'br
                                  'text
                                  'tt
                                  'link
                                  'strong
                                  'strike
                                  'subscript
                                  'superscript))))
    (.ret (<em> ss))))

;;; ----------------------------------------------------

(define-parser strike
  (.let (ss (.between (.is :strike)
                      (.is :strike)
                      (.many (.or 'br
                                  'text
                                  'tt
                                  'link
                                  'strong
                                  'em
                                  'subscript
                                  'superscript))))
    (.ret (<s> ss))))

;;; ----------------------------------------------------

(define-parser subscript
  (.let (ss (.between (.is :subscript)
                      (.is :subscript)
                      (.many (.or 'br
                                  'text
                                  'tt
                                  'link
                                  'strong
                                  'em
                                  'strike
                                  'superscript))))
    (.ret (<sub> ss))))

;;; ----------------------------------------------------

(define-parser superscript
  (.let (ss (.between (.is :superscript)
                      (.is :superscript)
                      (.many (.or 'br
                                  'text
                                  'tt
                                  'link
                                  'strong
                                  'em
                                  'strike
                                  'subscript))))
    (.ret (<sup> ss))))
