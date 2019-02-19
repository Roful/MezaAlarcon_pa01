
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


(library (ikarus.compiler)
  (export compile-core-expr-to-port 
          assembler-output optimize-cp
          current-primitive-locations eval-core
          current-core-eval compile-core-expr 
          expand expand/optimize expand/scc-letrec optimizer-output
          cp0-effort-limit cp0-size-limit optimize-level 
          perform-tag-analysis tag-analysis-output
          strip-source-info generate-debug-calls current-letrec-pass)
  (import 
    (rnrs hashtables)
    (ikarus system $fx)
    (ikarus system $pairs)
    (only (ikarus system $codes) $code->closure)
    (only (ikarus system $structs) $struct-ref $struct/rtd?)
    (except (ikarus)
        optimize-level debug-optimizer
        fasl-write optimize-cp
        compile-core-expr-to-port assembler-output
        current-primitive-locations eval-core
        cp0-size-limit cp0-effort-limit 
        expand/optimize expand/scc-letrec expand optimizer-output
        tag-analysis-output perform-tag-analysis
        current-core-eval current-letrec-pass)
    (ikarus include)
    (ikarus.fasl.write)
    (ikarus.intel-assembler))


(define strip-source-info (make-parameter #f))
(define generate-debug-calls (make-parameter #f))

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
   (name reg-conf frm-conf var-conf reg-move frm-move var-move
         loc index referenced global-loc))
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
(define-struct closure (code free* well-known?))
(define-struct funcall (op rand*))
(define-struct jmpcall (label op rand*))
(define-struct forcall (op rand*))
(define-struct codes (list body))
(define-struct assign (lhs rhs))
(define-struct mvcall (producer consumer))

(define-struct known (expr type))

(define-struct shortcut (body handler))

(define-struct fvar (idx))
(define-struct object (val))
(define-struct locals (vars body))
(define-struct nframe (vars live body))
(define-struct nfv (conf loc var-conf frm-conf nfv-conf))
(define-struct ntcall (target value args mask size))
(define-struct asm-instr (op dst src))
(define-struct disp (s0 s1))

;;; this define-structure definition for compatibility with the
;;; notation used in Oscar's thesis.
(define-syntax define-structure
  (lambda (stx) 
    (define (fmt ctxt)
      (lambda (str . args) 
        (datum->syntax ctxt 
          (string->symbol 
            (apply format str (map syntax->datum args))))))
    (syntax-case stx ()
      [(_ (name fields ...)) 
       #'(define-struct name (fields ...))]
      [(_ (name fields ...) ([others defaults] ...))
       (with-syntax ([(pred maker (getters ...) (setters ...))
                      (let ([fmt (fmt #'name)])
                        (list (fmt "~s?" #'name)
                              (fmt "make-~s" #'name)
                              (map (lambda (x) (fmt "~s-~s" #'name x))
                                   #'(fields ... others ...))
                              (map (lambda (x) (fmt "set-~s-~s!" #'name x))
                                   #'(fields ... others ...))))])
         #'(module (name pred getters ... setters ... maker)
             (module P (name pred getters ... setters ... maker)
               (define-struct name (fields ... others ...)))
             (module (maker)
               (define (maker fields ...)
                 (import P)
                 (maker fields ... defaults ...)))
             (module (name pred getters ... setters ...)
               (import P))))])))
;;;
(define-structure (prelex name operand)
  ([source-referenced?   #f]
   [source-assigned?     #f]
   [residual-referenced? #f]
   [residual-assigned?   #f]
   [global-location      #f]))

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

(define (unique-var name)
  (make-var name #f #f #f #f #f #f #f #f #f #f))

(define (recordize x)
  (define *cookie* (gensym))
  (define (gen-fml* fml*)
    (cond
      [(pair? fml*)
       (let ([v (make-prelex (car fml*) #f)])
         (putprop (car fml*) *cookie* v)
         (cons v (gen-fml* (cdr fml*))))]
      [(symbol? fml*)
       (let ([v (make-prelex fml* #f)])
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
  (define (lexical x) 
    (getprop x *cookie*))
  (define (get-fmls x args) 
    (define (matching? fmls args)
      (cond
        [(null? fmls) (null? args)]
        [(pair? fmls) (and (pair? args) (matching? (cdr fmls) (cdr args)))]
        [else #t]))
    (define (get-cls* x)
      (if (pair? x)
          (case (car x)
            [(case-lambda) (cdr x)]
            [(annotated-case-lambda) (cddr x)]
            [else '()])
          '()))
    (let f ([cls* (get-cls* x)])
      (cond
        [(null? cls*) '()]
        [(matching? (caar cls*) args)
         (caar cls*)]
        [else (f (cdr cls*))])))
  (define (make-global-set! lhs rhs)
    (make-funcall (make-primref '$init-symbol-value!)
      (list (make-constant lhs) rhs)))
  (define-syntax equal-case
    (lambda (x)
      (syntax-case x ()
         [(_ val clause* ...) 
          (with-syntax ([body 
                         (let f ([clause* #'(clause* ...)])
                           (syntax-case clause* (else)
                             [([else e e* ...]) 
                              #'(begin e e* ...)]
                             [([(datum* ...) e e* ...] . rest)
                              (with-syntax ([rest (f #'rest)])
                                #'(if (member t '(datum* ...))
                                      (begin e e* ...)
                                      rest))]))])
            #'(let ([t val]) body))])))

  (define (E-clambda-clause* cls* ctxt)
    (map
      (let ([ctxt (if (pair? ctxt) (car ctxt) #f)])
        (lambda (cls)
          (let ([fml* (car cls)] [body (cadr cls)])
            (let ([nfml* (gen-fml* fml*)])
              (let ([body (E body ctxt)])
                (ungen-fml* fml*)
                (make-clambda-case 
                  (make-case-info
                    (gensym)
                    (properize nfml*) 
                    (list? fml*)) 
                  body))))))
           cls*))
  (define (E-make-parameter mk-call args ctxt)
    (case (length args)
      [(1)
       (let ([val-expr (car args)]
             [t (gensym 't)]
             [x (gensym 'x)])
         (E `((lambda (,t) 
                (case-lambda
                  [() ,t]
                  [(,x) (set! ,t ,x)]))
              ,val-expr)
            ctxt))]
      [(2)
       (let ([val-expr (car args)]
             [guard-expr (cadr args)]
             [f (gensym 'f)]
             [t (gensym 't)]
             [t0 (gensym 't)]
             [x (gensym 'x)])
         (E `((case-lambda 
                [(,t ,f)
                 (if ((primitive procedure?) ,f)
                     ((case-lambda
                        [(,t0)
                         (case-lambda
                           [() ,t0]
                           [(,x) (set! ,t0 (,f ,x))])])
                      (,f ,t))
                     ((primitive die)
                        'make-parameter 
                        '"not a procedure"
                        ,f))])
              ,val-expr
              ,guard-expr)
            ctxt))]
      [else 
       (mk-call 
         (make-primref 'make-parameter)
         (map (lambda (x) (E x #f)) args))]))
  (define (E-app mk-call rator args ctxt)
    (equal-case rator
      [((primitive make-parameter)) (E-make-parameter mk-call args ctxt)]
      [else
       (let ([names (get-fmls rator args)])
         (mk-call 
           (E rator (list ctxt))
           (let f ([args args] [names names])
             (cond
               [(pair? names)
                (cons 
                  (E (car args) (car names))
                  (f (cdr args) (cdr names)))]
               [else
                (map (lambda (x) (E x #f)) args)]))))]))
  (define (E x ctxt)
    (cond
      [(pair? x)
       (equal-case (car x)
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
                 (set-prelex-source-assigned?! var #t)
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
                    (set-prelex-global-location! lhs loc))
                  nlhs* loc*)
                (let ([expr (make-rec*bind nlhs* (map E rhs* lhs*)
                               (let f ([lhs* nlhs*] [loc* loc*])
                                 (cond
                                   [(null? lhs*) (E body ctxt)]
                                   [(not (car loc*)) (f (cdr lhs*) (cdr loc*))]
                                   [else (f (cdr lhs*) (cdr loc*))])))])
                  (ungen-fml* lhs*)
                  expr))))]
         [(case-lambda)
          (let ([cls* (E-clambda-clause* (cdr x) ctxt)])
            (make-clambda (gensym) cls* #f #f
              (and (symbol? ctxt) ctxt)))]
         [(annotated-case-lambda)
          (let ([ae (cadr x)])
            (let ([cls* (E-clambda-clause* (cddr x) ctxt)])
              (make-clambda (gensym) cls* #f #f
                (cons 
                  (and (symbol? ctxt) ctxt)
                  (and (not (strip-source-info))
                       (annotation? ae)
                       (annotation-source ae))))))]
         [(lambda) 
          (E `(case-lambda ,(cdr x)) ctxt)]
         [(foreign-call)
          (let ([name (quoted-string (cadr x))] [arg* (cddr x)])
            (make-forcall name (map (lambda (x) (E x #f)) arg*)))]
         [(primitive)
          (let ([var (cadr x)])
            (make-primref var))]
         [(annotated-call) 
          (E-app
            (if (generate-debug-calls)
                (lambda (op rands)
                  (define (operator? x)
                    (struct-case x
                      [(primref x) 
                       (guard (con [(assertion-violation? con) #t])
                         (system-value x)
                         #f)]
                      [else #f]))
                  (define (get-src/expr ae)
                    (if (annotation? ae)
                        (cons (annotation-source ae) (annotation-stripped ae))
                        (cons #f (syntax->datum ae))))
                  (define src/expr 
                    (make-constant (get-src/expr (cadr x))))
                  (if (operator? op)
                      (make-funcall op rands)
                      (make-funcall (make-primref 'debug-call)
                        (cons* src/expr op rands))))
                make-funcall)
            (caddr x) (cdddr x) ctxt)]
         [else (E-app make-funcall (car x) (cdr x) ctxt)])]
      [(symbol? x)
       (cond
         [(lexical x) =>
          (lambda (var)
            (set-prelex-source-referenced?! var #t)
            var)]
         [else
          (make-funcall 
            (make-primref 'top-level-value) 
            (list (make-constant x)))])]
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
      [(known x t) `(known ,(E x) ,(T:description t))]
      [(code-loc x) `(code-loc ,x)]
      [(var x) (string->symbol (format ":~a" x))]
      [(prelex name) (string->symbol (format ":~a" name))]
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
       `( ;   label: ,(case-info-label info)
         ,(E-args (case-info-proper info) (case-info-args info))
         ,(E body))]
      [(clambda g cls* cp free)
       `(clambda (label: ,g) ; cp: ,(E cp) ) ;free: ,(map E free))
           ,@(map E cls*))]
      [(clambda label clauses free)
       `(code ,label . ,(map E clauses))]
      [(closure code free* wk?)
       `(closure ,@(if wk? '(wk) '()) ,(E code) ,(map E free*))]
      [(codes list body)
       `(codes ,(map E list)
          ,(E body))]
      [(funcall rator rand*) `(funcall ,(E rator) . ,(map E rand*))]
      [(jmpcall label rator rand*)
       `(jmpcall ,label ,(E rator) . ,(map E rand*))]
      [(forcall rator rand*) `(foreign-call ,rator . ,(map E rand*))]
      [(assign lhs rhs) `(set! ,(E lhs) ,(E rhs))]
      [(return x) `(return ,(E x))]
      [(new-frame base-idx size body)
       `(new-frame [base: ,base-idx]
                   [size: ,size]
          ,(E body))]
      [(frame-var idx) 
       (string->symbol (format "fv.~a" idx))]
      [(cp-var idx) 
       (string->symbol (format "cp.~a" idx))]
      [(save-cp expr)
       `(save-cp ,(E expr))]
      [(eval-cp check body)
       `(eval-cp ,check ,(E body))]
      [(call-cp call-convention label save-cp? rp-convention base-idx arg-count live-mask)
       `(call-cp [conv: ,call-convention]
                 [label: ,label]
                 [rpconv: ,(if (symbol? rp-convention)
                               rp-convention
                               (E rp-convention))]
                 [base-idx: ,base-idx]
                 [arg-count: ,arg-count]
                 [live-mask: ,live-mask])]
      [(tailcall-cp convention label arg-count)
       `(tailcall-cp ,convention ,label ,arg-count)]
      [(foreign-label x) `(foreign-label ,x)]
      [(mvcall prod cons) `(mvcall ,(E prod) ,(E cons))]
      [(fvar idx) (string->symbol (format "fv.~a" idx))]
      [(nfv idx) 'nfv]
      [(locals vars body) `(locals ,(map E vars) ,(E body))]
      [(asm-instr op d s)
       `(asm ,op ,(E d) ,(E s))]
      [(disp s0 s1)
       `(disp ,(E s0) ,(E s1))]
      [(nframe vars live body) `(nframe ;[vars: ,(map E vars)]
                                        ;[live: ,(map E live)]
                                  ,(E body))]
      [(shortcut body handler)
       `(shortcut ,(E body) ,(E handler))]
      [(ntcall target valuw args mask size)
       `(ntcall ,target ,size)]
      [else
       (if (symbol? x) 
           x
           "#<unknown>")]))
  (E x))


(define (unparse-pretty x)
  (define n 0)
  (define h (make-eq-hashtable))
  (define (Var x)
    (or (hashtable-ref h x #f)
        (let ([v (string->symbol (format "~a_~a" (prelex-name x) n))])
          (hashtable-set! h x v)
          (set! n (+ n 1))
          v)))
  (define (map f ls)
    (cond
      [(null? ls) '()]
      [else
       (let ([a (f (car ls))])
         (cons a (map f (cdr ls))))]))
  (define (E-args proper x)
    (if proper 
        (map Var x)
        (let f ([a (car x)] [d (cdr x)])
          (cond
            [(null? d) (Var a)]
            [else 
             (let ([a (Var a)])
               (cons a (f (car d) (cdr d))))]))))
  (define (clambda-clause x)
    (struct-case x
      [(clambda-case info body)
       (let ([args (E-args (case-info-proper info) (case-info-args info)) ])
         (list args (E body)))]))
  (define (build-let b* body)
    (cond
      [(and (= (length b*) 1)
            (pair? body)
            (or (eq? (car body) 'let*)
                (and (eq? (car body) 'let)
                     (= (length (cadr body)) 1))))
       (list 'let* (append b* (cadr body)) (caddr body))]
      [else 
       (list 'let b* body)]))
  (define (E x)
    (struct-case x
      [(constant c) `(quote ,c)]
      [(prelex) (Var x)]
      [(primref x) x]
      [(known x t) `(known ,(E x) ,(T:description t))]
      [(conditional test conseq altern) 
       (cons 'if (map E (list test conseq altern)))]
      [(primcall op arg*) (cons op (map E arg*))]
      [(bind lhs* rhs* body) 
       (let* ([lhs* (map Var lhs*)]
              [rhs* (map E rhs*)]
              [body (E body)])
         (import (only (ikarus) map))
         (build-let (map list lhs* rhs*) body))]
      [(fix lhs* rhs* body)
       (let* ([lhs* (map Var lhs*)]
              [rhs* (map E rhs*)]
              [body (E body)])
         (import (only (ikarus) map))
         (list 'letrec (map list lhs* rhs*) body))]
      [(recbind lhs* rhs* body)
       (let* ([lhs* (map Var lhs*)]
              [rhs* (map E rhs*)]
              [body (E body)])
         (import (only (ikarus) map))
         (list 'letrec (map list lhs* rhs*) body))] 
      [(rec*bind lhs* rhs* body)
       (let* ([lhs* (map Var lhs*)]
              [rhs* (map E rhs*)]
              [body (E body)])
         (import (only (ikarus) map))
         (list 'letrec* (map list lhs* rhs*) body))] 
      [(seq e0 e1) 
       (cons 'begin
          (let f ([e0 e0] [e* (list e1)])
            (struct-case e0
              [(seq e00 e01)
               (f e00 (cons e01 e*))]
              [else
               (let ([x (E e0)])
                 (if (null? e*)
                     (list x)
                     (cons x (f (car e*) (cdr e*)))))])))]
      [(clambda g cls* cp free)
       (let ([cls* (map clambda-clause cls*)])
         (cond
           [(= (length cls*) 1) (cons 'lambda (car cls*))]
           [else (cons 'case-lambda cls*)]))]
      [(funcall rator rand*) 
       (let ([rator (E rator)])
         (cons rator (map E rand*)))]
      [(forcall rator rand*) `(foreign-call ,rator . ,(map E rand*))]
      [(assign lhs rhs) `(set! ,(E lhs) ,(E rhs))]
      [(foreign-label x) `(foreign-label ,x)]
      [else x]))
  (E x))

(define open-mvcalls (make-parameter #t))

(define (optimize-direct-calls x)
  (define who 'optimize-direct-calls)
  (define (make-conses ls)
    (cond
      [(null? ls) (make-constant '())]
      [else 
       (make-funcall (make-primref 'cons) 
         (list (car ls) (make-conses (cdr ls))))]))      
  (define (properize lhs* rhs*)
    (cond
      [(null? lhs*) (error who "improper improper")]
      [(null? (cdr lhs*)) 
       (list (make-conses rhs*))]
      [else (cons (car rhs*) (properize (cdr lhs*) (cdr rhs*)))]))
  (define (inline-case cls rand*)
    (struct-case cls
      [(clambda-case info body)
       (struct-case info
         [(case-info label fml* proper)
          (if proper
              (and (fx= (length fml*) (length rand*))
                   (make-bind fml* rand* body))
              (and (fx<= (length fml*) (length rand*))
                   (make-bind fml* (properize fml* rand*) body)))])]))
  (define (try-inline cls* rand* default)
    (cond
      [(null? cls*) default]
      [(inline-case (car cls*) rand*)]
      [else (try-inline (cdr cls*) rand* default)]))
  (define (inline mk rator rand*)
    (define (valid-mv-consumer? x)
      (struct-case x
        [(clambda L cases F)
         (and (fx= (length cases) 1)
              (struct-case (car cases)
                [(clambda-case info body)
                 (struct-case info
                   [(case-info L args proper) proper])]))]
        [else #f]))
    (define (single-value-consumer? x)
      (struct-case x
        [(clambda L cases F)
         (and (fx= (length cases) 1)
              (struct-case (car cases)
                [(clambda-case info body)
                 (struct-case info
                   [(case-info L args proper)
                    (and proper (fx= (length args) 1))])]))]
        [else #f])) 
    (define (valid-mv-producer? x)
      (struct-case x
        [(funcall) #t]
        [(conditional) #f]
        [(bind lhs* rhs* body) (valid-mv-producer? body)]
        [else #f] ;; FIXME BUG
        ))
    (struct-case rator
      [(clambda g cls*)
       (try-inline cls* rand*
          (mk rator rand*))]
      [(primref op)
       (case op
         ;;; FIXME HERE
         [(call-with-values)
          (cond
            [(and (open-mvcalls) (fx= (length rand*) 2))
             (let ([producer (inline (car rand*) '())] 
                   [consumer (cadr rand*)])
               (cond
                 [(single-value-consumer? consumer)
                  (inline consumer (list producer))]