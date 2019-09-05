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

(define-lexer quickdoc-lexer (s)
  ("@([^:]+):(%N+)"     (values :tag (mapcar #'strip-tag $*)))
  ("@(%N+)"             (values :tag (mapcar #'strip-tag $*)))

  ;; skip newlines
  ("%n+"                (values :next-token))

  ;; anything else begins the document body
  (""                   (swap-lexer s 'body-lexer :body)))

;;; ----------------------------------------------------

(define-lexer body-lexer (s)

  ;; headers
  ("=%s(%N+)"           (values :h1 (strip-header $1)))
  ("==%s(%N+)"          (values :h2 (strip-header $1)))
  ("===%s(%N+)"         (values :h3 (strip-header $1)))
  ("====%s(%N+)"        (values :h4 (strip-header $1)))
  ("=====%s(%N+)"       (values :h5 (strip-header $1)))
  ("======%s(%N+)"      (values :h6 (strip-header $1)))

  ;; horizontal rules
  ("%-%-%-%s*(%N*)"     (values :hr (strip-rule $1)))

  ;; pre-formatted text
  (":(?%s(%N+))?"       (values :pre (or $1 "")))

  ;; block quotes
  (">(?%s(%N+))?"       (values :quote (or $1 "")))

  ;; table records
  ("%|%s(%N+)"          (values :tr (parse-table-row $1)))

  ;; list items
  ("%*%s(%N+)"          (values :ul $1))
  ("#%s(%N+)"           (values :ol $1))

  ;; embedded images and video
  ("!%s(%N+)"           (values :img= (parse-link $1)))
  ("!<%s(%N+)"          (values :img< (parse-link $1)))
  ("!>%s(%N+)"          (values :img> (parse-link $1)))

  ;; skip single newlines
  ("%r?%n%n%n+"         (values :div))
  ("%r?%n"              (values :next-token))

  ;; formatted text
  ("(%N+)"              (values :p $1)))

;;; ----------------------------------------------------

(define-lexer span-lexer (s)

  ;; forced line break
  ("\\%r?%n"             (values :br))

  ;; unicode and escaped characters
  ("\\u(%x%x%x%x)"       (values :text (unicode-char $1)))
  ("\\(.)"               (values :text $1))

  ;; hard-coded unicode symbols
  ("%(tm%)"              (values :text #\u+2122))
  ("%(R%)"               (values :text #\u+00ae))
  ("%(C%)"               (values :text #\u+00a9))
  ("%(1/4%)"             (values :text #\u+00bc))
  ("%(1/2%)"             (values :text #\u+00bd))
  ("%(3/4%)"             (values :text #\u+00be))
  ("%(o%)"               (values :text #\u+00b0))
  ("%(%+/%-%)"           (values :text #\u+00b1))

  ;; teletyped characters
  ("`"                   (push-lexer s 'tt-lexer :tt))

  ;; external links
  ("%[%[([^%]]*)%]%]"    (values :link (parse-link $1)))

  ;; emphasized characters
  ("%*%*"                (values :strong))
  ("__"                  (values :em))
  ("~~"                  (values :strike))
  ("%^%^"                (values :superscript))
  (",,"                  (values :subscript))

  ;; non-terminal characters
  (".[^~\\%[%(%*_`%^,]*" (values :text $$))

  ;; end of line/buffer exits
  ("%r?%n|$"))

;;; ----------------------------------------------------

(define-lexer tt-lexer (s)

  ;; end of stream or teletype
  ("$"                   (pop-lexer s :-tt))
  ("`"                   (pop-lexer s :-tt))

  ;; escaped characters
  ("\\(.)"               (values :text $1))

  ;; everything else is just characters
  (".[^\\`]*"            (values :text $$)))

;;; ----------------------------------------------------

(defun strip-tag (s)
  "Remove all whitespace from a tag name/content."
  (string-trim '(#\space #\tab) s))

;;; ----------------------------------------------------

(defun strip-header (s)
  "Remove all whitespace from a header caption."
  (string-trim '(#\= #\space #\tab) s))

;;; ----------------------------------------------------

(defun strip-rule (s)
  "Remove all whitespace from a horizontal rule caption."
  (string-trim '(#\- #\space #\tab) s))

;;; ----------------------------------------------------

(defun unicode-char (xxxx)
  "Convert a 4-character hex value to a unicode character."
  (string (code-char (parse-integer xxxx :radix 16))))

;;; ----------------------------------------------------

(defun parse-table-row (row)
  "Read a table row as a csv record."
  (with-input-from-string (s row)
    (read-record s)))

;;; ----------------------------------------------------

(defun parse-link (link)
  "Extract the optional, alternate name for a url."
  (with-re-match (m (match-re #r"([^%s|]+)%s*(?%|%s*(.*))?" link))
    (list $1 $2)))
