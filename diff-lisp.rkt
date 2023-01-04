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
(require "parse-lisp.rkt")
(require "diff.rkt")
(require "htmlize.rkt")


(define *keywords*
  '(define defun defvar lambda cond if else
     let let* let-values let*-values
     while for define-syntax syntax-rules
     define-minor-mode defmacro defn))

(define *defs*
  '(define defun defvar define-syntax define-minor-mode
    defmacro defn))


(define get-keyword
  (lambda (node)
    (match node
      [(Node type _ _ elts _ _)
       (cond
        [(pair? elts)
         (let ([sym (get-symbol (car elts))])
           (cond
            [(memq sym *keywords*) sym]
            [else #f]))]
        [else #f])]
      [_ #f])))


;; Try to get the keyword of the sexp if it is not token, comment, str
;; and char.
(set-get-type
  (lambda (node)
    (cond
     [(token? node) 'to