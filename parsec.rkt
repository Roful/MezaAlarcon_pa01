
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



;----------------------------------------
;               options
;----------------------------------------

(define *left-recur-detection* #f)



;-------------------------------------------------------------
;                 parser combinator library
;-------------------------------------------------------------

;; s-expression settings
;; please override for other languages.
(define *delims* (list "("  ")"  "["  "]"  "{"  "}" "'"  "`"  ","))
(define *line-comment* (list ";"))
(define *comment-start* "")
(define *comment-end* "")
(define *operators*  '())
(define *quotation-marks* '(#\" #\'))
(define *lisp-char* (list "#\\" "?\\"))
(define *significant-whitespaces* '())


(define (set-delims x)
  (set! *delims* x))

(define (set-line-comment x)
  (set! *line-comment* x))

(define (set-comment-start x)
  (set! *comment-start* x))

(define (set-comment-end x)
  (set! *comment-end* x))

(define (set-operators x)
  (set! *operators* x))

(define (set-quotation-marks x)
  (set! *quotation-marks* x))

(define (set-lisp-char x)
  (set! *lisp-char* x))

(define (set-significant-whitespaces x)
  (set! *significant-whitespaces* x))



;-------------------------------------------------------------
;                          scanner
;-------------------------------------------------------------

(define whitespace?  char-whitespace?)
(define alpha?       char-alphabetic?)
(define digit?       char-numeric?)

(define set-alpha
  (lambda (x)
    (set! alpha? x)))


; Is char c a delimeter?
(define delim?
  (lambda (c)
    (member (char->string c) *delims*)))


(define id?
  (lambda (s)
    (cond
     [(= 0 (string-length s)) #f]
     [(or (alpha? (string-ref s 0))
          (eq? #\_ (string-ref s 0)))
      (let loop ([i 1])
        (cond
         [(>= i (string-length s)) #t]
         [else
          (let ([c (string-ref s i)])
            (cond
             [(alpha? c) (loop (add1 i))]
             [(digit? c) (loop (add1 i))]
             [(char=? c #\_) (loop (add1 i))]
             [else #f]))]))]
     [else #f])))


(define numeral?
  (lambda (s)
    (cond
     [(= 0 (string-length s)) #f]
     [(digit? (string-ref s 0)) #t
      ;; (let loop ([i 1])
      ;;   (cond
      ;;    [(>= i (string-length s)) #t]
      ;;    [else
      ;;     (let ([c (string-ref s i)])
      ;;         (cond
      ;;          [(digit? c) (loop (add1 i))]
      ;;          [(char=? c #\.) (loop (add1 i))]
      ;;          [else #f]))]))
]
     [else #f])))



(define start-with
  (lambda (s start prefix)
    (let* ([prefix-str (if (char? prefix)
                           (char->string prefix)
                           prefix)]
           [len (string-length prefix-str)])
      (cond
       [(= len 0) #f]
       [(< (string-length s) (+ start len)) #f]
       [(string=? (substring s start (+ start len)) prefix-str)
        prefix]
       [else #f]))))



(define start-with-one-of
  (lambda (s start prefixes)
    (cond
     [(null? prefixes) #f]
     [(start-with s start (car prefixes))
      (car prefixes)]
     [else
      (start-with-one-of s start (cdr prefixes))])))

; (start-with-one-of "+>>=" 0 (list ">" #\+))



(define find-next
  (lambda (s start pred)
    (cond
     [(<= (string-length s) start) #f]
     [(pred s start) start]
     [else
      (find-next s (add1 start) pred)])))



; Find the first delim that match the start of s
(define find-delim
  (lambda (s start)
    (start-with-one-of s start *delims*)))



(define find-operator
  (lambda (s start)
    (start-with-one-of s start *operators*)))

; (find-operator ">> x" 0)



(define scan
  (lambda (s)
    (define scan1
      (lambda (s start)
        (cond
         [(= start (string-length s)) (values 'eof start)]

         [(start-with-one-of s start *significant-whitespaces*)
          (values (Node 'newline start (add1 start) '() #f #f)
                  (add1 start))]

         [(whitespace? (string-ref s start))
          (scan1 s (add1 start))]

         [(start-with-one-of s start *line-comment*) ; line comment
          (let ([line-end (find-next s start
                                     (lambda (s start)
                                       (eq? (string-ref s start) #\newline)))])
            (values (Node 'comment
                          start
                          (add1 line-end)
                          (substring s start line-end)
                          #f #f)
                    line-end))]

         [(start-with s start *comment-start*) ; block comment
          (let* ([line-end (find-next s start
                                      (lambda (s start)
                                        (start-with s start *comment-end*)))]
                 [end (+ line-end (string-length *comment-end*))])
            (values (Node 'comment
                          start
                          end
                          (substring s start end)
                          #f #f)
                    end))]

         [(find-delim s start) =>
          (lambda (delim)
            (let ([end (+ start (string-length delim))])
              (values (Node 'token start end delim #f #f)
                      end)))]

         [(find-operator s start) =>
          (lambda (op)
            (let ([end (+ start (string-length op))])
              (values (Node 'token start end op #f #f)
                      end)))]

         [(start-with-one-of s start *quotation-marks*)   ; string
          (let ([reg-match (or (regexp-match (regexp "^\"(\\\\.|[^\"])*\"")
                                             s start)
                               (regexp-match (regexp "^\'(\\\\.|[^\'])*\'")
                                             s start))])
            (cond
             [(not reg-match)
              (fatal 'scan "string match error")]
             [else
              (let* ([len (string-length (car reg-match))]
                     [end (+ start len)])
                (values (Node 'str start end (car reg-match) #f #f)
                        end))]))]

         [(start-with-one-of s start *lisp-char*)         ; scheme/elisp char
          (cond
           [(<= (string-length s) (+ 2 start))
            (error 'scan-string "reached EOF while scanning char")]
           [else
            (let ([end
                   (let loop ([end (+ 3 start)])
                     (cond
                      [(or (whitespace? (string-ref s end))
                           (delim? (string-ref s end)))
                       end]
                      [else (loop (add1 end))]))])
              (values (Node 'char start end (string-ref s (sub1 end)) #f #f)
                      end))])]

         [else                        ; identifier or number
          (let loop ([pos start] [chars '()])
            (cond
             [(or (<= (string-length s) pos)
                  (whitespace? (string-ref s pos))
                  (find-delim s pos)
                  (find-operator s pos))
              (let ([text (list->string (reverse chars))])
                (values (Node 'token start pos text #f #f)
                        pos))]
             [else
              (loop (add1 pos) (cons (string-ref s pos) chars))]))])))

    (let loop ([start 0] [toks '()])
      (letv ([(tok newstart) (scan1 s start)])
        (cond
         [(eq? tok 'eof)
          (reverse toks)]
         [else
          (loop newstart (cons tok toks))])))))






;-------------------------------------------------------------
;                           parser
;-------------------------------------------------------------

(define onstack?
  (lambda (u v stk)
    (let loop ([stk stk] [trace '()])
      (cond
       [(null? stk) #f]
       [(and (eq? u (car (car stk)))
             (eq? v (cdr (car stk))))
        (reverse (cons (car stk) trace))]
       [else
        (loop (cdr stk) (cons (car stk) trace))]))))



(define ext
  (lambda (u v stk)
    (cond
     [(not *left-recur-detection*) stk]
     [else
      (cons (cons u v) stk)])))



(define stack->string
  (lambda (stk)
    (let ([ps (map
               (lambda (x) (format "~a" (car x)))
               stk)])
      (string-join ps "\n"))))

; (display (stack->string (onstack? 'x 'y '((u . v) (x . y) (w . t)))))



;; apply parser on toks, check for left-recurson if
;; *left-recur-detection* is enabled.
(define apply-check
  (lambda (parser toks stk ctx)
    (cond
     [(and *left-recur-detection*
           (onstack? parser toks stk))
      => (lambda (t)
           (fatal 'apply-check
                  "left-recursion detected     \n"
                  "parser: " parser           "\n"
                  "start token: " (car toks)  "\n"
                  "stack trace: " (stack->string t)))]
     [else
      ((parser) toks (ext parser toks stk) ctx)])))




;------------------ parser combinators --------------------
(define @seq
  (lambda ps
    (lambda ()
      (lambda (toks stk ctx)
        (let loop ([ps ps] [toks toks] [nodes '()])
          (cond
           [(null? ps)
            (values (apply append (reverse nodes)) toks)]
           [else
            (letv ([(t r) (apply-check (car ps) toks stk ctx)])
              (cond
               [(not t)
                (values #f #f)]
               [else
                (loop (cdr ps) r (cons t nodes))]))]))))))

(define set-seq
  (lambda (x)
    (set! @seq x)))



;; removes phantoms
(define @...
  (lambda ps
    (let ([parser ((apply @seq ps))])
      (lambda ()
        (lambda (toks stk ctx)
          (letv ([(t r) (parser toks stk ctx)])
            (cond
             [(not t) (values #f #f)]
             [else
              (values (filter (negate phantom?) t) r)])))))))


; (((@seq)) (scan "ok"))



(define @or
  (lambda ps
    (lambda ()
      (lambda (toks stk ctx)
        (let loop ([ps ps])
             (cond
              [(null? ps)
               (values #f #f)]
              [else
               (letv ([(t r) (apply-check (car ps) toks stk ctx)])
                 (cond
                  [(not t)
                   (loop (cdr ps))]
                  [else
                   (values t r)]))]))))))


; (((@or ($$ "foo") ($$ "bar"))) (scan "bar foo"))



(define @=
  (lambda (type . ps)
    (let ([parser ((apply @seq ps))])
      (lambda ()
        (lambda (toks stk ctx)
          (letv ([(t r) (parser toks stk ctx)])
            (cond
             [(not t) (values #f #f)]
             [(not type)
              (values (filter (negate phantom?) t) r)]
             [(null? t)
              (values (list (Node type
                                  (Node-start (car toks))
                                  (Node-start (car toks))
                                  '()
                                  #f #f))
                      r)]
             [else
              (values (list (Node type
                                  (Node-start (car t))
                                  (Node-end (last t))
                                  (filter (negate phantom?) t)
                                  #f #f))
                      r)])))))))



(define @*
  (lambda ps
    (let ([parser ((apply @... ps))])
      (lambda ()
        (lambda (toks stk ctx)
          (let loop ([toks toks] [nodes '()])
            (cond
             [(null? toks)
              (values (apply append (reverse nodes)) '())]
             [else
              (letv ([(t r) (parser toks stk ctx)])
                (cond
                 [(not t)
                  (values (apply append (reverse nodes)) toks)]
                 [else
                  (loop r (cons t nodes))]))])))))))


; ($eval (@* ($$ "ok")) (scan "ok ok ok"))


;; similar to @*, but takes only one parser and will not
;; make a sequence by invoking @seq
(define @*^
  (lambda (p)
    (lambda ()
      (lambda (toks stk ctx)
        (let loop ([toks toks] [nodes '()])
          (cond
           [(null? toks)
            (values (apply append (reverse nodes)) '())]
           [else
            (letv ([(t r) ((p) toks stk ctx)])
              (cond
               [(not t)
                (values (apply append (reverse nodes)) toks)]
               [else
                (loop r (cons t nodes))]))]))))))


(define @+
  (lambda (p)
    (@... p (@* p))))

; (((@+ ($$ "ok"))) (scan "ok ok ok"))


(define @?
  (lambda ps
    (@or (apply @... ps) $none)))


; (((@? ($$ "x"))) (scan "x y z"))


;; negation - will fail if ps parses successfully.
(define @!
  (lambda ps
    (let ([parser ((apply @... ps))])
      (lambda ()
        (lambda (toks stk ctx)
          (letv ([(t r) (parser toks stk ctx)])
            (cond
             [(not t) (values (list (car toks)) (cdr toks))]
             [else (values #f #f)])))))))