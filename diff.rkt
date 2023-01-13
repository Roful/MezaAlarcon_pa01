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


(provide (all-defined-out))


;-------------------------------------------------------------
;                      parameters
;-------------------------------------------------------------

;; The minimum size of a node to be considered as moved. Shouldn't be
;; too small, otherwise small deleted names may appear in a very
;; distant place!
(define *move-size* 5)


;; Similar to *move-size*, but this number is used for internal moves inside a
;; named body (for example a function). This number can be smaller than
;; *move-size*, usually set to 2 for maxmum accuracy without much noise.
(define *inner-move-size* 2)


;; Only memoize the diff of nodes of size larger than this number.
;; This effectively reduces memory usage.
(define *memo-node-size* 2)


;; Are we in moving phase?
;; This is internal switch used by the diff algorithm.
;; Do not modify by handl.
(define *moving* #f)



;;------------------ frames utils --------------------
(define deframe
  (lambda (node)
    (match node
      [(Node 'frame _ _ elts _ _)
       (apply append (map deframe elts))]
     [else (list node)])))


(define deframe-change
  (lambda (change)
    (cond
     [(ins? change)
      (apply append
             (map ins (deframe (Change-new change))))]
     [(del? change)
      (apply append
             (map del (deframe (Change-old change))))]
     [else (list change)])))


(define extract-frame
  (lambda (node1 node2 type)
    (match node1
      [(Node type1 start1 end1 elts1 size ctx)
       (let ([frame-elts (filter (lambda (x)
                                   (not (eq? x node2)))
                                 elts1)])
         (type (Node 'frame start1 start1 frame-elts (- size (node-size node2)) ctx)))]
      [_ fatal 'extract-frame "I only accept Node"])))


;; (define n1 (Token "ok" 0 1))
;; (define n2 (Expr 'ok 0 2 (list n1 (Token "bar" 1 2))))
;; (map deframe-change (extract-frame n2 n1 ins))



;--------------------------------------------------------