
;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus compiler)
  (export compile-core-expr-to-port 
          assembler-output
          current-primitive-locations eval-core)
  (import 
    (rnrs hashtables)
    (ikarus system $fx)
    (ikarus system $pairs)
    (only (ikarus system $codes) $code->closure)
    (only (ikarus system $structs) $struct-ref $struct/rtd?)
    (except (ikarus)
        fasl-write
        compile-core-expr-to-port assembler-output
        current-primitive-locations eval-core)
    (ikarus fasl write)
    (ikarus intel-assembler))


(define-syntax struct-case
  (lambda (x)
    (define (enumerate fld* i)
      (syntax-case fld* ()
        [() #'()]
        [(x . x*) 
         (with-syntax ([i i] [i* (enumerate #'x* (fx+ i 1))])
           #'(i . i*))]))
    (define (generate-body ctxt cls*)
      (syntax-case cls* (else)
        [() (with-syntax ([x x]) #'(error #f "unmatched " v 'x))]
        [([else b b* ...])  #'(begin b b* ...)]
        [([(rec-name rec-field* ...) b b* ...] . rest) (identifier? #'rec-name)
         (with-syntax ([altern (generate-body ctxt #'rest)]
                       [(id* ...) (enumerate #'(rec-field* ...) 0)]
                       [rtd #'(type-descriptor rec-name)])
          #'(if ($struct/rtd? v rtd)
                (let ([rec-field* ($struct-ref v id*)] ...)
                  b b* ...)
                altern))]))
    (syntax-case x ()
      [(_ expr cls* ...)
       (with-syntax ([body (generate-body #'_ #'(cls* ...))])
         #'(let ([v expr]) body))])))




(define (remq1 x ls)
  (cond
    [(null? ls) '()]
    [(eq? x (car ls)) (cdr ls)]
    [else
     (let ([t (remq1 x (cdr ls))])
       (cond
         [(eq? t (cdr ls)) ls]
         [else (cons (car ls) t)]))]))

(define (singleton x) (list x))

(define (union s1 s2)
  (define (add* s1 s2)
    (cond
     [(null? s1) s2]
     [else (add (car s1) (add* (cdr s1) s2))]))
  (define (add x s)
    (cond
     [(memq x s) s]
     [else (cons x s)]))
  (cond
   [(null? s1) s2]
   [(null? s2) s1]
   [else (add* s1 s2)]))

(define (difference s1 s2)
  (define (rem* s1 s2)
    (cond
     [(null? s1) s2]
     [else (remq1 (car s1) (rem* (cdr s1) s2))]))
  (cond
   [(null? s1) '()]
   [(null? s2) s1]
   [else (rem* s2 s1)]))


  
(define-struct constant (value))
(define-struct code-loc (label))
(define-struct foreign-label (label))
(define-struct var 
   (name assigned referenced 
         reg-conf frm-conf var-conf reg-move frm-move var-move
         loc index global-loc))
(define-struct cp-var (idx))
(define-struct frame-var (idx))
(define-struct new-frame (base-idx size body))
(define-struct save-cp (loc))
(define-struct eval-cp (check body))
(define-struct return (value))
(define-struct call-cp
  (call-convention label save-cp? rp-convention base-idx arg-count live-mask))
(define-struct tailcall-cp (convention label arg-count))
(define-struct primcall (op arg*))
(define-struct primref (name))
(define-struct conditional (test conseq altern))
(define-struct interrupt-call (test handler))
(define-struct bind (lhs* rhs* body))
(define-struct recbind (lhs* rhs* body))
(define-struct rec*bind (lhs* rhs* body))
(define-struct fix (lhs* rhs* body))

(define-struct seq (e0 e1))
(define-struct case-info (label args proper))
(define-struct clambda-case (info body))
(define-struct clambda (label cases cp free name))
(define-struct closure (code free*))
(define-struct funcall (op rand*))
(define-struct jmpcall (label op rand*))
(define-struct forcall (op rand*))
(define-struct codes (list body))
(define-struct assign (lhs rhs))
(define-struct mvcall (producer consumer))



(define-struct shortcut (body handler))

(define-struct fvar (idx))
(define-struct object (val))
(define-struct locals (vars body))
(define-struct nframe (vars live body))
(define-struct nfv (conf loc var-conf frm-conf nfv-conf))
(define-struct ntcall (target value args mask size))
(define-struct asm-instr (op dst src))
(define-struct disp (s0 s1))

(define mkfvar
  (let ([cache '()])
    (lambda (i)
      (cond
        [(fixnum? i)
         (cond
           [(assv i cache) => cdr]
           [else
            (let ([fv (make-fvar i)])
              (set! cache (cons (cons i fv) cache))
              fv)])]
        [else (error 'mkfvar "not a fixnum" i)]))))

(define (unique-var x)
  (make-var (gensym x) #f #f #f #f #f #f #f #f #f #f #f))

(define (recordize x)
  (define *cookie* (gensym))
  (define (gen-fml* fml*)
    (cond
      [(pair? fml*)
       (let ([v (unique-var (car fml*))])
         (putprop (car fml*) *cookie* v)
         (cons v (gen-fml* (cdr fml*))))]
      [(symbol? fml*)
       (let ([v (unique-var fml*)])
         (putprop fml* *cookie* v)
         v)]
      [else '()]))
  (define (ungen-fml* fml*)
    (cond
      [(pair? fml*)
       (remprop (car fml*) *cookie*)
       (ungen-fml* (cdr fml*))]
      [(symbol? fml*)
       (remprop fml* *cookie*)]))
  (define (properize fml*)
    (cond
      [(pair? fml*)
       (cons (car fml*) (properize (cdr fml*)))]
      [(null? fml*) '()]
      [else (list fml*)]))
  (define (quoted-sym x)
    (if (and (list? x)
             (fx= (length x) 2)
             (eq? 'quote (car x))
             (symbol? (cadr x)))
        (cadr x)
        (error 'quoted-sym "not a quoted symbol" x)))
  (define (quoted-string x)
    (if (and (list? x)
             (fx= (length x) 2)
             (eq? 'quote (car x))
             (string? (cadr x)))
        (cadr x)
        (error 'quoted-string "not a quoted string" x)))
  (define (Var x)
    (or (getprop x *cookie*) 
        (error 'recordize "unbound" x)))
  (define (lexical x) 
    (getprop x *cookie*))
  (define (get-fmls x args) 
    (define (matching? fmls args)
      (cond
        [(null? fmls) (null? args)]
        [(pair? fmls) (and (pair? args) (matching? (cdr fmls) (cdr args)))]
        [else #t]))
    (cond
      [(and (pair? x) (eq? (car x) 'case-lambda))
       (let f ([cls* (cdr x)])
         (cond
           [(null? cls*) '()]
           [(matching? (caar cls*) args) 
            (caar cls*)]
           [else (f (cdr cls*))]))]
      [else '()]))
  (define (make-global-set! lhs rhs)
    (make-funcall (make-primref '$init-symbol-value!)
      (list (make-constant lhs) rhs)))
  (define (E x ctxt)
    (cond
      [(pair? x)
       (case (car x)
         [(quote) (make-constant (cadr x))]
         [(if) 
          (make-conditional 
            (E (cadr x) #f)
            (E (caddr x) ctxt)
            (E (cadddr x) ctxt))]
         [(set!)
          (let ([lhs (cadr x)] [rhs (caddr x)])
            (cond
              [(lexical lhs) => 
               (lambda (var) 
                 (make-assign var (E rhs lhs)))]
              [else (make-global-set! lhs (E rhs lhs))]))] 
         [(begin)
          (let f ([a (cadr x)] [d (cddr x)])
            (cond
              [(null? d) (E a ctxt)]
              [else
               (make-seq (E a #f) (f (car d) (cdr d)))]))]
         [(letrec)
          (let ([bind* (cadr x)] [body (caddr x)])
            (let ([lhs* (map car bind*)]
                  [rhs* (map cadr bind*)])
              (let ([nlhs* (gen-fml* lhs*)])
                (let ([expr (make-recbind nlhs* (map E rhs* lhs*) (E body ctxt))])
                  (ungen-fml* lhs*)
                  expr))))]
         [(letrec*)
          (let ([bind* (cadr x)] [body (caddr x)])
            (let ([lhs* (map car bind*)]
                  [rhs* (map cadr bind*)])
              (let ([nlhs* (gen-fml* lhs*)])
                (let ([expr (make-rec*bind nlhs* (map E rhs* lhs*) (E body ctxt))])
                  (ungen-fml* lhs*)
                  expr))))]
         [(library-letrec*)
          (let ([bind* (cadr x)] [body (caddr x)])
            (let ([lhs* (map car bind*)]
                  [loc* (map cadr bind*)]
                  [rhs* (map caddr bind*)])
              (let ([nlhs* (gen-fml* lhs*)])
                (for-each 
                  (lambda (lhs loc) 
                    (set-var-global-loc! lhs loc))
                  nlhs* loc*)
                (let ([expr (make-rec*bind nlhs* (map E rhs* lhs*)
                               (let f ([lhs* nlhs*] [loc* loc*])
                                 (cond
                                   [(null? lhs*) (E body ctxt)]
                                   [(not (car loc*)) (f (cdr lhs*) (cdr loc*))]
                                   [else
                                    (make-seq 
                                      (make-global-set! (car loc*) (car lhs*))
                                      (f (cdr lhs*) (cdr loc*)))])))])
                  (ungen-fml* lhs*)
                  expr))))]
         [(case-lambda)
          (let ([cls*
                 (map
                   (lambda (cls)
                     (let ([fml* (car cls)] [body (cadr cls)])
                       (let ([nfml* (gen-fml* fml*)])
                         (let ([body (E body #f)])
                           (ungen-fml* fml*)
                           (make-clambda-case 
                             (make-case-info
                               (gensym)
                               (properize nfml*) 
                               (list? fml*)) 
                             body)))))
                   (cdr x))])
            (make-clambda (gensym) cls* #f #f ctxt))]
         [(lambda) 
          (E `(case-lambda ,(cdr x)) ctxt)]
         [(foreign-call)
          (let ([name (quoted-string (cadr x))] [arg* (cddr x)])
            (make-forcall name (map (lambda (x) (E x #f)) arg*)))]
         [(primitive)
          (let ([var (cadr x)])
            (make-primref var))]
         [else
          (let ([names (get-fmls (car x) (cdr x))])
            (make-funcall 
              (E (car x) #f) 
              (let f ([arg* (cdr x)] [names names])
                (cond
                  [(pair? names)
                   (cons 
                     (E (car arg*) (car names))
                     (f (cdr arg*) (cdr names)))]
                  [else
                   (map (lambda (x) (E x #f)) arg*)]))))])]
      [(symbol? x)
       (or (lexical x) 
           (make-funcall 
             (make-primref 'top-level-value) 
             (list (make-constant x))))]
      [else (error 'recordize "invalid expression" x)]))
  (E x #f))

(define (unparse x)
  (define (E-args proper x)
    (if proper 
        (map E x)
        (let f ([a (car x)] [d (cdr x)])
          (cond
            [(null? d) (E a)]
            [else (cons (E a) (f (car d) (cdr d)))]))))
  (define (E x)
    (struct-case x
      [(constant c) `(quote ,c)]
      [(code-loc x) `(code-loc ,x)]
      [(var x) (string->symbol (format "v:~a" x))]
      [(primref x) x]
      [(conditional test conseq altern) 
       `(if ,(E test) ,(E conseq) ,(E altern))]
      [(interrupt-call e0 e1)
       `(interrupt-call ,(E e0) ,(E e1))]
      [(primcall op arg*) `(,op . ,(map E arg*))]
      [(bind lhs* rhs* body) 
       `(let ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body))]
      [(recbind lhs* rhs* body) 
       `(letrec ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body))]
      [(rec*bind lhs* rhs* body) 
       `(letrec* ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body))]
      ;[(library-recbind lhs* loc* rhs* body) 
      ; `(letrec ,(map (lambda (lhs loc rhs) (list (E lhs) loc (E rhs))) 
      ;                lhs* loc* rhs*)
      ;    ,(E body))]
      [(fix lhs* rhs* body) 
       `(fix ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body))]
      [(seq e0 e1) 
       (let ()
         (define (f x ac)
           (struct-case x
             [(seq e0 e1) (f e0 (f e1 ac))]
             [else (cons (E x) ac)]))
         (cons 'begin (f e0 (f e1 '()))))]
      [(clambda-case info body)
       `(,(E-args (case-info-proper info) (case-info-args info))
          ,(E body))]
      [(clambda g cls* cp free)
       `(,g (case-lambda . ,(map E cls*)))]
      [(clambda label clauses free)
       `(code ,label . ,(map E clauses))]
      [(closure code free*)
       `(closure ,(E code) ,(map E free*))]