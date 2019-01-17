
;; yDiff - a language-aware tool for comparing programs
;; Copyright (C) 2011 Yin Wang (yinwang0@gmail.com)


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



(load "utils.ss")



;-------------------------------------------------------------
;                      parameters
;-------------------------------------------------------------

;; The ratio of cost/size that we consider two nodes to be
;; "similar", so as to perform a heuristic move (that will
;; cut running time by a lot.) But this number should be
;; small enough otherwise everything will be considered to
;; be moves! Set to a small number for accuracy.
(define *move-ratio* 0)


;; The minimum size of a node to be considered for moves.
;; Shouldn't be too small, otherwise small deletec names
;; will appear in a very distant place! This number should
;; be larger than *min-frame-size* otherwise it will not be
;; effective because substructural diff will do it.
(define *move-size* 5)


;; The depth limit for detecting substructural moves. If
;; this is set, we will ignore nodes that are deeper than
;; this number. The minimum is 7 to be useful.
(define *move-depth* 5)

;; How many iterations do we go for moves? This parameter
;; should be large enough so that we can discover enough
;; moves. The algorithm is guaranteed to terminate, but it
;; should be be set to some small number if we found that it
;; takes too much time to terminate. This value should be
;; larger than 7 in order to work for normal programs.
(define *move-iteration* 1000)


;; How large must a node be considered as container for
;; another node? If this is set to a big number, we will
;; ignore nodes that are smaller than the number. This
;; number shouldn't be too small, otherwise too many
;; spurious moves will be detected! For example, a deleted
;; "int" could be moved to a very far place! Setting it to a
;; bigger number also reduces running time (by a great
;; amount) because less substructural moves are considered.
;; Set it to a larger number will cause loss of
;; substructural changes, but greatly reduces time to run.
(define *min-frame-size* 3)


;; How deep must the frames be for us to consider them as
;; moves? This affects only already extracted frames, which
;; may be considered to be moves to other extracted frames.
;; Set it large will not necessarily lower accuracy, but
;; improves performance.
(define *min-frame-depth* 2)


;; How long must a string be in order for us to use
;; string-dist function, which is costly when used on long
;; strings but the most accurate method to use. This
;; parameter affects strings/comments only. We use
;; string-dist for all Tokens.
(define *max-string-len* 200)


;; only memoize the diff of nodes of size larger than this
;; number
(define *memo-node-size* 2)



(define *keywords* '())
(define *keyword-exchange* '())
(define *defs* '())



;-------------------------------------------------------------
;                      utilities
;-------------------------------------------------------------

(define qs
  (lambda (x)
    (string-append "'" (number->string x) "'")))


(define line
  (lambda (port . s)
    (display (string-append (apply string-append s) "\n") port)))



;-------------------------------------------------------------
;                      data types
;-------------------------------------------------------------

(struct Change (orig cur cost type) #:transparent)
(struct Tag (tag idx start) #:transparent)


(define ins?
  (lambda (c)
    (not (Change-orig c))))

(define del?
  (lambda (c)
    (not (Change-cur c))))

(define mod?
  (lambda (c)
    (and (Change-orig c) (Change-cur c))))



;----------------- utils for creating changes ----------------
(define total
  (lambda (node1 node2)
    (let ([size1 (node-size node1)]
          [size2 (node-size node2)])
      (values (append (del-node node1) (ins-node node2))
              (+ size1 size2)))))

(define del-node
  (lambda (node)
    (let ([size (node-size node)])
      (list (Change node #f size 'del)))))

(define ins-node
  (lambda (node)
    (let ([size (node-size node)])
      (list (Change #f node size 'ins)))))



(define disassemble-frame
  (lambda (node)
    (cond
     [(and (Expr? node) (eq? 'frame (Expr-type node)))
      (apply append (map disassemble-frame (Expr-elts node)))]
     [else (list node)])))


(define disassemble-change
  (lambda (change)
    (cond
     [(ins? change)
      (apply append
             (map ins-node
                  (disassemble-frame (Change-cur change))))]
     [(del? change)
      (apply append
             (map del-node
                  (disassemble-frame (Change-orig change))))]
     [else (list change)])))


(define extract-frame
  (lambda (node1 node2)
    (match node1
      [(Expr type1 elts1 start1 end1)
       (let ([frame-elts (filter (lambda (x)
                                   (not (eq? x node2)))
                                 elts1)])
         (Expr 'frame frame-elts start1 start1))]
      [_ fatal 'extract-frame "I only accept Expr"])))



(define extract-ins-frame
  (lambda (node1 node2)
    (let ([frame (extract-frame node1 node2)])
      (cond
       [(not frame) '()]
       [else
        (ins-node frame)]))))



(define extract-del-frame
  (lambda (node1 node2)
    (let ([frame (extract-frame node1 node2)])
      (cond
       [(not frame) '()]
       [else
        (del-node frame)]))))



;; (define n1 (Token "ok" 0 1))

;; (define n2 (Expr 'ok (list n1 (Token "bar" 1 2)) 0 2))

;; (map disassemble-change (extract-ins-frame n2 n1))




(define ins-node-except
  (lambda (node1 node2)
    (let ([nodes (map (lambda (x)
                        (if (not (eq? x node2))
                            (ins-node x)
                            '()))
                      (Expr-elts node1))])
      (apply append nodes))))


(define del-node-except
  (lambda (node1 node2)
    (let ([nodes (map (lambda (x)
                        (if (not (eq? x node2))
                            (del-node x)
                            '()))
                      (Expr-elts node1))])
      (apply append nodes))))



(define mod-node
  (lambda (node1 node2 cost)
    (list (Change node1 node2 cost 'mod))))

(define mov-node
  (lambda (node1 node2 cost)
    (list (Change node1 node2 cost 'mov))))

(define mod->mov
  (lambda (c)
    (match c
     [(Change node1 node2 cost 'mod)
      (Change node1 node2 cost 'mov)]
     [other other])))




;------------------ operations on nodes ---------------------

(define node-equal?
  (lambda (node1 node2)
    (cond
     [(and (null? node1) (null? node2)) #t]
     [(and (Str? node1) (Str? node2))
      (and (equal? (Str-s node1) (Str-s node2)))]
     [(and (Comment? node1) (Comment? node2))
      (and (equal? (Comment-text node1) (Comment-text node2)))]
     [(and (Char? node1) (Char? node2))
      (and (equal? (Char-c node1) (Char-c node2)))]
     [(and (Token? node1) (Token? node2))
      (and (equal? (Token-text node1)
                   (Token-text node2)))]
     [(and (Expr? node1) (Expr? node2))
      (and (eq? (Expr-type node1)
                (Expr-type node2))
           (node-equal? (Expr-elts node1)
                        (Expr-elts node2)))]
     [(and (pair? node1) (pair? node2))
      (and (node-equal? (car node1) (car node2))
           (node-equal? (cdr node1) (cdr node2)))]
     [else #f])))



(define keyword-exchangeable?
  (lambda (k1 k2)
    (cond
     [(eq? k1 k2) #t]
     [(assq k1 *keyword-exchange*)
      => (lambda (p)
           (cond
            [(memq k2 (cdr p)) #t]
            [else #f]))]
     [else #f])))



(define keywords-equal?
  (lambda (node1 node2)
    (and (eq? (Expr-type node1)
              (Expr-type node2))
         (not (keywords-differ? node1 node2)))))



(define keywords-differ?
  (lambda (exp1 exp2)
    (let ([key1 (and (not (null? (Expr-elts exp1)))
                     (get-symbol (car (Expr-elts exp1))))]
          [key2 (and (not (null? (Expr-elts exp2)))
                     (get-symbol (car (Expr-elts exp2))))])
      (cond
       [(and key1 key2
             (or (memq key1 *keywords*)
                 (memq key2 *keywords*))
             (not (keyword-exchangeable? key1 key2)))
        #t]
       [else #f]))))



(define get-symbol
  (lambda (node)
    (cond
     [(Token? node)
      (string->symbol (Token-text node))]
     [else #f])))



(define same-def?
  (lambda (e1 e2)
    (cond
      [(and (Expr? e1) (Expr? e2))
       (let ([elts1 (Expr-elts e1)]
             [elts2 (Expr-elts e2)])
         (cond
          [(and (> (length elts1) 1)
                (> (length elts2) 1)
                (memq (get-symbol (car elts1)) *defs*)
                (memq (get-symbol (car elts2)) *defs*))
           (eq? (get-symbol (cadr elts1))
                (get-symbol (cadr elts2)))]
          [else #f]))]
      [else #f])))

;; (same-def? (car (parse-scheme "(define f 1)"))
;;            (car (parse-scheme "(define g 1)")))



(define different-def?
  (lambda (e1 e2)
    (cond
      [(and (Expr? e1) (Expr? e2))
       (let ([elts1 (Expr-elts e1)]
             [elts2 (Expr-elts e2)])
         (cond
          [(and (> (length elts1) 1)
                (> (length elts2) 1)
                (memq (get-symbol (car elts1)) *defs*)
                (memq (get-symbol (car elts2)) *defs*))
           (not (eq? (get-symbol (cadr elts1))
                     (get-symbol (cadr elts2))))]
          [else #f]))]
      [else #f])))

;; (different-def? (car (parse-scheme "(define f 1)"))
;;                 (car (parse-scheme "(let f 1)")))



;; whether two nodes are similar given the cost
(define similar?
  (lambda (node1 node2 c)
    (<= c (* *move-ratio* (+ (node-size node1)
                             (node-size node2))))))


(define *node-size-hash* (make-hasheq))

(define node-size
  (lambda (node)
    (define memo
      (lambda (v)
        (if (> v 1)
            (hash-set! *node-size-hash* node v)
            (void))
        v))
    (cond
     [(pair? node)
      (apply + (map node-size node))]
     [(or (Token? node) (Str? node) (Char? node)) 1]
     [(Expr? node)
      (cond
       [(hash-has-key? *node-size-hash* node)
        (hash-ref *node-size-hash* node)]
       [else
        (memo (node-size (Expr-elts node)))])]
     [else 0])))


(define node-depth
  (lambda (node)
    (cond
     [(null? node) 0]
     [(pair? node)
      (apply max (map node-depth node))]
     [(Expr? node)
      (add1 (node-depth (Expr-elts node)))]
     [else 0])))


; (node-depth (parse-scheme "(lambda (x (x (y)) (y)) x)"))

;; (same-def? (parse-scheme "(define f (x 1))")
;;            (parse-scheme "(define f 2"))



(define uid
  (let ([count 1]
        [table (box '())])
    (lambda (node)
      (let ([p (assq node (unbox table))])
        (cond
         [(not p)
          (let ([id count])
            (set! count (add1 count))
            (set-box! table (cons `(,node . ,id) (unbox table)))
            id)]
         [else
          (cdr p)])))))



(define similarity
  (lambda (change)
    (let ([total (+ (node-size (Change-orig change))
                    (node-size (Change-cur change)))])
      (cond
       [(or (= 0 total) (= 0 (Change-cost change)))
        "100%"]
       [else
        (string-append
         (real->decimal-string
          (* 100 (- 1.0 (/ (Change-cost change) total))) 1)
         "%")]))))



;-------------------------------------------------------------
;                        diff stuff
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



(define diff-string
  (lambda (string1 string2 node1 node2)
    (cond
     [(or (> (string-length string1) *max-string-len*)
          (> (string-length string2) *max-string-len*))
      (cond
       [(equal? string1 string2)
        (values (mod-node node1 node2 0) 0)]
       [else
        (total node1 node2)])]
     [else
      (let ([cost (string-dist string1 string2)])
        (values (mod-node node1 node2 cost) cost))])))




;--------------------- main node diff function ----------------------
(define diff-node
  (lambda (node1 node2 depth move?)

    (define memo
      (lambda (v1 v2)
        (if (and (> (node-size node1) *memo-node-size*)
                 (> (node-size node2) *memo-node-size*))