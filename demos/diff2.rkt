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


;; How long must a string be in order for us to use string-dist
;; function, which is costly when used on long strings but the most
;; accurate method to use. Currently this parameter is set to 0,
;; effective disables all LCS string comparison. This improves
;; performance while not sacrificing accuracy because the algorithm is
;; AST based.
(define *max-string-len* 0)


;; Only memoize the diff of nodes of size larger than this number.
;; This effectively reduces memory usage.
(define *memo-node-size* 2)




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



;-------------------------------------------------------------
;                       diff proper
;-------------------------------------------------------------

; 2-D memoization table
(define make-table
  (lambda (dim1 dim2)
    (let ([vec (make-vector (add1 dim1))])
      (let loop ([n 0])
        (cond
         [(= n (vector-length vec)) vec]
         [else
          (vector-set! vec n (make-vector (add1 dim2) #f))
          (loop (add1 n))])))))


(define table-ref
  (lambda (t x y)
    (let ([row (vector-ref t x)])
      (vector-ref row y))))


(define table-set!
  (lambda (t x y v)
    (let ([row (vector-ref t x)])
      (vector-set! row y v))))



;---------------- string distance function -----------------

;; string distance is no longer used because string=? saffice to
;; compare strings in ASTs. Retain it here for possible later uses.
(define string-dist
  (lambda (s1 s2)
    (let* ([len1 (string-length s1)]
           [len2 (string-length s2)]
           [t (make-table len1 len2)]
           [char-dist (dist1 t s1 0 s2 0)])
      (cond
       [(= 0 (+ len1 len2)) 0]
       [else
        (/ (* 2.0 char-dist) (+ len1 len2))]))))


(define dist1
  (lambda (table s1 start1 s2 start2)
    (define memo
      (lambda (value)
        (table-set! table start1 start2 value)
        value))
    (cond
     [(table-ref table start1 start2)
      => (lambda (cached) cached)]
     [(= start1 (string-length s1))
      (memo (- (string-length s2) start2))]
     [(= start2 (string-length s2))
      (memo (- (string-length s1) start1))]
     [else
      (let* ([c1 (string-ref s1 start1)]
             [c2 (string-ref s2 start2)]
             [d0 (cond
                  [(char=? c1 c2) 0]
                  [(char=? (char-downcase c1)
                           (char-downcase c2)) 1]
                  [else 2])]
             [d1 (+ d0 (dist1 table s1 (add1 start1) s2 (add1 start2)))]
             [d2 (+ 1 (dist1 table s1 (add1 start1) s2 start2))]
             [d3 (+ 1 (dist1 table s1 start1 s2 (add1 start2)))])
        (memo (min d1 d2 d3)))])))





;--------------------- the primary diff function -------------------
(define diff-node
  (lambda (node1 node2 move?)

    (define memo
      (lambda (v1 v2)
        (and (> (node-size node1) *memo-node-size*)
             (> (node-size node2) *memo-node-size*)
             (hash-put! *diff-hash* node1 node2 (cons v1 v2)))
        (values v1 v2)))

    (define try-extract
      (lambda (changes cost)
        (cond
         [(or (not move?)
              (zero? cost))
          (memo changes cost)]
         [else
          (letv ([(m c) (diff-extract node1 node2 move?)])
            (cond
             [(not m)
              (memo changes cost)]
             [else
              (memo m c)]))])))


    (diff-progress 1) ;; progress bar

    (cond
     [(hash-get *diff-hash* node1 node2)
      => (lambda (cached)
           (values (car cached) (cdr cached)))]
     [(and (character? node1) (character? node2))
      (diff-string (char->string (Node-elts node1))
                   (char->string (Node-elts node2))
                   node1 node2)]
     [(and (str? node1) (str? node2))
      (diff-string (Node-elts node1) (Node-elts node2) node1 node2)]
     [(and (comment? node1) (comment? node2))
      (