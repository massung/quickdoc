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

;;; ----------------------------------------------------

(defun render-quickdoc (doc &optional (stream *standard-output*))
  ""
  (with-slots (meta body)
      doc
    (<body> (render-nodes body))))

(defun render-nodes (args)
  ""
  (mapcar #'(lambda (n) (print n) (apply #'render-node n)) args))

(defun render-node (type &rest args)
  ""
  (case type
    (:br (<br>))

    ;; headings
    (:h1 (<h1> :style "clear:both" args))
    (:h2 (<h2> :style "clear:both" args))
    (:h3 (<h3> :style "clear:both" args))
    (:h4 (<h4> :style "clear:both" args))
    (:h5 (<h5> :style "clear:both" args))
    (:h6 (<h6> :style "clear:both" args))

    ;; horizontal rules
    (:hr (<div> :class "hr" :style "clear:both"))

    ;; block quotes
    (:quote (<blockquote> :style "clear:both" (render-nodes args)))

    ;; pre-formatted text
    (:pre (<pre> :style "clear:both" args))

    ;; tables
    (:table (let ((trs (loop
                          for i from 0
                          for row in args

                          ;; first row is header
                          collect (if (zerop i)
                                      (<th> (apply #'render-node row))
                                    (<td> (apply #'render-node row))))))
              (<table> (<tbody> (mapcar #'<tr> trs)))))

    ;; lists
    (:ul (<ul> :style "clear:both"
               (loop for n in args collect (<li> (apply #'render-node n)))))
    (:ol (<ol> :style "clear:both"
               (loop for n in args collect (<li> (apply #'render-node n)))))

    ;; links
    (:link (destructuring-bind (url &optional alt)
               args
             (<a> :href url :alt (or alt url))))

    ;; centered images
    (:img= (destructuring-bind (url &optional caption)
               args
             (<div> :style "clear:both"
                    (<center> (<img> :src url)
                              (when caption
                                (<div> :class "caption" caption))))))

    ;; floating left image
    (:img< (destructuring-bind (url &optional caption)
               args
             (<div> :class "left"
                    :style "float:left"
                    (<center> (<img> :src url)
                              (when caption
                                (<div> :class "caption" caption))))))

    ;; floating right image
    (:img> (destructuring-bind (url &optional caption)
               args
             (<div> :class "right"
                    :style "float:right"
                    (<center> (<img> :src url)
                              (when caption
                                (<div> :class "caption" caption))))))

    ;; teletype
    (:tt (<tt> args))

    ;; paragraph
    (:p (<p> (render-nodes args)))

    ;; spans
    (:strong (<strong> (render-nodes args)))
    (:em (<em> (render-nodes args)))
    (:strike (<s> (render-nodes args)))
    (:superscript (<sup> (render-nodes args)))
    (:subscript (<sub> (render-nodes args)))

    ;; anything else is as-is
    (otherwise args)))
