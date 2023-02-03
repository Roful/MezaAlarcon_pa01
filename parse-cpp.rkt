;; ydiff - a language-aware tool for comparing programs
;; Copyright (C) 2011-2013 Yin Wang (yinwang0@gmail.com)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


#lang racket

(require "structs.rkt")
(require "utils.rkt")
(require "parsec.rkt")

(provide parse-cpp)




;-------------------------------------------------------------
;                     scanner settings
;-------------------------------------------------------------

(define set-parameters
  (lambda ()
    (set-delims (list "("  ")"  "["  "]"  "{"  "}" ","  "`"  ";" "#"))

    (set-operators
     (list
      "<<="  ">>="   "->*"  "..."
      "&&"  "||"  ">>"  "<<"  "++"  "--"
      "=="  "!="  ">="  "<="  "+="  "-="  "*="  "/="  "^="  "&="  "|="
      "->"  ".*"  "::"
      "="  "+"  "-"  "*"  "/"  "%"  "<"  ">"  "!"  ":"  "?"  "."
      "^"  "|"  "&"  "~"
      ))

    (set-line-comment  (list "//"))
    (set-comment-start  "/*")
    (set-comment-end    "*/")
    (set-quotation-marks  '(#\" #\'))
    (set-significant-whitespaces
     (list #\newline #\linefeed #\u2028 #\u2029))))




;-------------------------------------------------------------
;                          parsers
;-------------------------------------------------------------

;; literals
(:: $id
    ($pred
     (lambda (t)
       (and (token? t)
            (id? (Node-elts t))))))


(::= $identifier 'identifier
     (@? ($$ "::"))
     (@* $id (@* $type-parameter) ($$ "::"))
     (@= 'id (@? ($$ "~")) $id))


;; (::= $identifier 'identifier
;;      (@? ($$ "::")) $scope-resolution (@? ($$ "~")) $id)


(:: $numeral-literal
    ($pred
     (lambda (t)
       (and (token? t)
            (numeral? (Node-elts t))))))

(:: $char-literal ($pred character?))
(:: $string-literal ($pred str?))
(:: $newline ($pred newline?))
(:: $comment ($pred comment?))


;; delimeters
(::  |,|   (@_ ","))
(::  |;|   (@~ ";"))
(::  |:|   (@_ ":"))
(::  |(|   (@~ "("))
(::  |)|   (@~ ")"))
(::  |[|   (@~ "["))
(::  |]|   (@~ "]"))
(::  |{|   (@~ "{"))
(::  |}|   (@~ "}"))

(::  |\n|  ($glob^ (@*^ $newline)))
(::  |;\n| (@or |;| |\n|))


(define old-seq @seq)

(set-seq
  (lambda ps
    (let ([psj (j