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

(deflexer span-lexer

  ;; newlines are hard breaks
  ("\\%n+"               (values :br))

  ;; unicode characters
  ("\\u(%x%x%x%x)"       (let ((c (code-char (parse-integer $1 :radix 16))))
                           (values :text c)))

  ;; escaped characters
  ("\\(.)"               (values :text $1))

  ;; teletype formatting
  ("`"                   (push-lexer 'tt-lexer :+tt))

  ;; special symbols
  ("%(tm%)"              (values :text #\u+2122))
  ("%(R%)"               (values :text #\u+00ae))
  ("%(C%)"               (values :text #\u+00a9))
  ("%(1/4%)"             (values :text #\u+00bc))
  ("%(1/2%)"             (values :text #\u+00bd))
  ("%(3/4%)"             (values :text #\u+00be))
  ("%(o%)"               (values :text #\u+00b0))
  ("%(%+/%-%)"           (values :text #\u+00b1))

  ;; emphasis
  ("%*%*"                (values :strong))
  ("__"                  (values :em))
  ("~~"                  (values :strike))
  ("%^%^"                (values :superscript))
  (",,"                  (values :subscript))

  ;; links
  ("%[%["                (push-lexer 'link-lexer :+link))

  ;; non-terminal characters
  (".[^~\\%[%(%*_`%^,]*" (values :text $$)))

(deflexer tt-lexer

  ;; end of stream or teletype
  ("$"                   (pop-lexer :-tt))
  ("`"                   (pop-lexer :-tt))

  ;; escaped characters
  ("\\(.)"               (values :chars $1))

  ;; everything else is just characters
  (".[^\\`]*"            (values :chars $$)))

(deflexer link-lexer
          
  ;; end of stream, line, or link
  ("$"                   (pop-lexer :-link))
  ("%]%]"                (pop-lexer :-link))

  ;; alternate text
  ("%|"                  (swap-lexer 'alt-lexer :alt))
  
  ;; everything else is the link
  (".[^|%]]*"            (values :url $$)))

(deflexer alt-lexer

  ;; end of stream, line, or link
  ("$"                   (pop-lexer :-link))
  ("%]%]"                (pop-lexer :-link))

  ;; everything else is the alternate text
  (".[^%]]*"             (values :text $$)))

(defparser span-parser
  ((start p) $1)

  ;; collection of spans
  ((p span p) `(,$1 ,@$2))
  ((p))

  ;; spans are emphasis or elements
  ((span e) $1)
  ((span strong) $1)
  ((span em) $1)
  ((span strike) $1)
  ((span super) $1)
  ((span sub) $1)

  ;; content spans
  ((e br) $1)
  ((e text) $1)
  ((e tt) $1)
  ((e link) $1)

  ;; hard breaks
  ((br :br)
   (make-br-node))

  ;; plain text
  ((text :text)
   (make-text-node :text $1))

  ;; monospace text
  ((tt :+tt /tt)
   (make-tt-node :text (format nil "~{~a~}" $2)))

  ;; unformatted text
  ((/tt :chars /tt) `(,$1 ,@$2))
  ((/tt :-tt))

  ;; internal and external links
  ((link :+link :url :-link)
   (make-link-node :url $2 :alt $2))
  ((link :+link :url :alt :text :-link)
   (make-link-node :url $2 :alt $4))

  ;; emphasis spans
  ((strong :strong /strong) (make-strong-node :spans $2))
  ((em :em /em) (make-em-node :spans $2))
  ((strike :strike /strike) (make-strike-node :spans $2))
  ((super :superscript /super) (make-superscript-node :spans $2))
  ((sub :subscript /sub) (make-subscript-node :spans $2))

  ;; strong spans
  ((/strong e /strong) `(,$1 ,@$2))
  ((/strong em /strong) `(,$1 ,@$2))
  ((/strong strike /strong) `(,$1 ,@$2))
  ((/strong super /strong) `(,$1 ,@$2))
  ((/strong sub /strong) `(,$1 ,@$2))
  ((/strong :strong))
  ((/strong :error))

  ;; em spans
  ((/em e /em) `(,$1 ,@$2))
  ((/em strong /em) `(,$1 ,@$2))
  ((/em strike /em) `(,$1 ,@$2))
  ((/em super /em) `(,$1 ,@$2))
  ((/em sub /em) `(,$1 ,@$2))
  ((/em :em))
  ((/em :error))

  ;; strike spans
  ((/strike e /strike) `(,$1 ,@$2))
  ((/strike strong /strike) `(,$1 ,@$2))
  ((/strike em /strike) `(,$1 ,@$2))
  ((/strike super /strike) `(,$1 ,@$2))
  ((/strike sub /strike) `(,$1 ,@$2))
  ((/strike :strike))
  ((/strike :error))

  ;; superscript spans
  ((/super e /super) `(,$1 ,@$2))
  ((/super strong /super) `(,$1 ,@$2))
  ((/super em /super) `(,$1 ,@$2))
  ((/super strike /super) `(,$1 ,@$2))
  ((/super sub /super) `(,$1 ,@$2))
  ((/super :superscript))
  ((/super :error))

  ;; subscript spans
  ((/sub e /sub) `(,$1 ,@$2))
  ((/sub strong /sub) `(,$1 ,@$2))
  ((/sub em /sub) `(,$1 ,@$2))
  ((/sub strike /sub) `(,$1 ,@$2))
  ((/sub super /sub) `(,$1 ,@$2))
  ((/sub :subscript))
  ((/sub :error)))
