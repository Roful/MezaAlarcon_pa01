
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
  (define (inline rator rand*)
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
          (make-funcall rator rand*))]
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
                 [(and (valid-mv-consumer? consumer)
                       (valid-mv-producer? producer))
                  (make-mvcall producer consumer)]
                 [else 
                  (make-funcall rator rand*)]))]
            [else
             (make-funcall rator rand*)])]
         [else
          (make-funcall rator rand*)])]
      [else (make-funcall rator rand*)]))
  (define (Expr x)
    (struct-case x
      [(constant) x]
      [(var) x]
      [(primref) x]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Expr body))]
      [(recbind lhs* rhs* body)
       (make-recbind lhs* (map Expr rhs*) (Expr body))]
      [(rec*bind lhs* rhs* body)
       (make-rec*bind lhs* (map Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (make-conditional 
         (Expr test)
         (Expr conseq)
         (Expr altern))]
      [(seq e0 e1) 
       (make-seq (Expr e0) (Expr e1))]
      [(clambda g cls* cp free name)
       (make-clambda g
         (map (lambda (x)
                (struct-case x
                  [(clambda-case info body)
                   (make-clambda-case info (Expr body))]))
              cls*)
         cp free name)]
      [(funcall rator rand*)
       (inline (Expr rator) (map Expr rand*))]
      [(forcall rator rand*) 
       (make-forcall rator (map Expr rand*))]
      [(assign lhs rhs)
       (make-assign lhs (Expr rhs))]
      ;[(library-recbind lhs* loc* rhs* body)
      ; (make-library-recbind lhs* loc* (map Expr rhs*) (Expr body))]
      [else (error who "invalid expression" (unparse x))]))
  (Expr x))


(define simple-primitives 
  ;;; primitives that are side-effect-free
  ;;; FIXME: surely something must go here, no?
  '())

(define (optimize-letrec x)
  (define who 'optimize-letrec)
  (define (extend-hash lhs* h ref)
    (for-each (lambda (lhs) (hashtable-set! h lhs #t)) lhs*)
    (lambda (x)
      (unless (hashtable-ref h x #f)
        (hashtable-set! h x #t)
        (ref x))))
  (define (E* x* ref comp)
    (cond
      [(null? x*) '()]
      [else
       (cons (E (car x*) ref comp)
             (E* (cdr x*) ref comp))]))  
  (define (do-rhs*-old i lhs* rhs* ref comp vref vcomp)
    (cond
      [(null? rhs*) '()]
      [else
       (let ([h (make-eq-hashtable)])
         (let ([ref
                (lambda (x)
                  (unless (hashtable-ref h x #f)
                    (hashtable-set! h x #t)
                    (ref x)
                    (when (memq x lhs*)
                      (vector-set! vref i #t))))]
               [comp
                (lambda ()
                  (vector-set! vcomp i #t)
                  (comp))])
           (cons (E (car rhs*) ref comp)
                 (do-rhs* (fxadd1 i) lhs* (cdr rhs*) ref comp vref vcomp))))])) 
  (define (do-rhs* i lhs* rhs* ref comp vref vcomp)
    (cond
      [(null? rhs*) '()]
      [else
       (let ([h (make-eq-hashtable)]
             [rest (do-rhs* (fxadd1 i) lhs* (cdr rhs*) ref comp vref vcomp)])
         (let ([ref
                (lambda (x)
                  (unless (hashtable-ref h x #f)
                    (hashtable-set! h x #t)
                    (ref x)
                    (when (memq x lhs*)
                      (vector-set! vref i #t))))]
               [comp
                (lambda ()
                  (vector-set! vcomp i #t)
                  (comp))])
           (cons (E (car rhs*) ref comp) rest)))]))
  (define (partition-rhs* i lhs* rhs* vref vcomp)
    (cond
      [(null? lhs*) (values '() '() '() '() '() '())]
      [else
       (let-values 
         ([(slhs* srhs* llhs* lrhs* clhs* crhs*)
           (partition-rhs* (fxadd1 i) (cdr lhs*) (cdr rhs*) vref vcomp)]
          [(lhs rhs) (values (car lhs*) (car rhs*))])
         (cond
           [(var-assigned lhs) 
            (values slhs* srhs* llhs* lrhs* (cons lhs clhs*) (cons rhs crhs*))]
           [(clambda? rhs)
            (values slhs* srhs* (cons lhs llhs*) (cons rhs lrhs*) clhs* crhs*)]
           [(or (vector-ref vref i) (vector-ref vcomp i))
            (values slhs* srhs* llhs* lrhs* (cons lhs clhs*) (cons rhs crhs*))]
           [else
            (values (cons lhs slhs*) (cons rhs srhs*) llhs* lrhs* clhs* crhs*)]
           ))]))
  (define (do-recbind lhs* rhs* body ref comp letrec?) 
    (let ([h (make-eq-hashtable)]
          [vref (make-vector (length lhs*) #f)]
          [vcomp (make-vector (length lhs*) #f)])
      (let* ([ref (extend-hash lhs* h ref)]
             [body (E body ref comp)])
        (let ([rhs* (do-rhs* 0 lhs* rhs* ref comp vref vcomp)])
          (let-values ([(slhs* srhs* llhs* lrhs* clhs* crhs*)
                        (partition-rhs* 0 lhs* rhs* vref vcomp)])
            ;(unless (null? clhs*)
            ;  (printf "CLHS* = ~s\n" (map unparse clhs*)))
            (let ([void* (map (lambda (x) (make-constant (void))) clhs*)])
              (make-bind slhs* srhs*
                (make-bind clhs* void*
                  (make-fix llhs* lrhs*
                    (if letrec?
                        (let ([t* (map (lambda (x) (unique-var 'tmp)) clhs*)])
                          (make-bind t* crhs*
                            (build-assign* clhs* t* body)))
                        (build-assign* clhs* crhs* body)))))))))))
  (define (build-assign* lhs* rhs* body)
    (cond
      [(null? lhs*) body]
      [else
       (make-seq
         (make-assign (car lhs*) (car rhs*))
         (build-assign* (cdr lhs*) (cdr rhs*) body))]))
  (define (E x ref comp)
    (struct-case x
      [(constant) x]
      [(var) (ref x) x]
      [(assign lhs rhs)
       (set-var-assigned! lhs #t)
       (ref lhs)
       (make-assign lhs (E rhs ref comp))]
      [(primref) x]
      [(bind lhs* rhs* body)
       (let ([rhs* (E* rhs* ref comp)])
         (let ([h (make-eq-hashtable)])
           (let ([body (E body (extend-hash lhs* h ref) comp)])
             (make-bind lhs* rhs* body))))]
      [(recbind lhs* rhs* body)
       (if (null? lhs*)
           (E body ref comp)
           (do-recbind lhs* rhs* body ref comp #t))]
      [(rec*bind lhs* rhs* body)
       (if (null? lhs*)
           (E body ref comp)
           (do-recbind lhs* rhs* body ref comp #f))] 
      [(conditional e0 e1 e2)
       (make-conditional (E e0 ref comp) (E e1 ref comp) (E e2 ref comp))]
      [(seq e0 e1) (make-seq (E e0 ref comp) (E e1 ref comp))]
      [(clambda g cls* cp free name)
       (make-clambda g
         (map (lambda (x)
                (struct-case x
                  [(clambda-case info body)
                   (let ([h (make-eq-hashtable)])
                     (let ([body (E body (extend-hash (case-info-args info) h ref) void)])
                       (make-clambda-case info body)))]))
              cls*)
         cp free name)]
      [(funcall rator rand*)
       (let ([rator (E rator ref comp)] [rand* (E* rand* ref comp)])
         (struct-case rator
           [(primref op)
            (unless (memq op simple-primitives)
              (comp))]
           [else
            (comp)])
         (make-funcall rator rand*))]
      [(mvcall p c)
       (let ([p (E p ref comp)] [c (E c ref comp)])
         (comp)
         (make-mvcall p c))]
      [(forcall rator rand*) 
       (make-forcall rator (E* rand* ref comp))]
      [else (error who "invalid expression" (unparse x))]))
  (E x (lambda (x) (error who "free var found" x))
       void))



(define (uncover-assigned/referenced x)
  (define who 'uncover-assigned/referenced)
  (define (Expr* x*)
    (for-each Expr x*))
  (define (init-var x)
    (set-var-assigned! x #f)
    (set-var-referenced! x #f))
  (define (Expr x)
    (struct-case x
      [(constant) (void)]
      [(var) (set-var-referenced! x #t)]
      [(primref) (void)]
      [(bind lhs* rhs* body)
       (for-each init-var lhs*)
       (begin (Expr body) (Expr* rhs*))]
      [(fix lhs* rhs* body)
       (for-each init-var lhs*)
       (Expr* rhs*)
       (Expr body)
       (when (ormap var-assigned lhs*)
         (error who "a fix lhs is assigned"))]
      [(conditional test conseq altern)
       (begin (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) (begin (Expr e0) (Expr e1))]
      [(clambda g cls*)
       (for-each
         (lambda (cls)
           (struct-case cls
             [(clambda-case info body)
              (for-each init-var (case-info-args info))
              (Expr body)]))
         cls*)]
      [(primcall rator rand*) (Expr* rand*)]
      [(funcall rator rand*)
       (begin (Expr rator) (Expr* rand*))]
      [(mvcall p c) (begin (Expr p) (Expr c))]
      [(forcall rator rand*) (Expr* rand*)]
      [(assign lhs rhs)
       (set-var-assigned! lhs #t)
       (Expr rhs)]
      [else (error who "invalid expression" (unparse x))]))
  (Expr x)
  x)


#|FIXME:missing-optimizations
  111 cadr
  464 $record/rtd?
  404 memq
  249 map
  114 not
  451 car
  224 syntax-error
  248 $syntax-dispatch
  237 pair?
  125 length
  165 $cdr
  137 $car
  805 $record-ref
  181 fixnum?
  328 null?
  136 fx-
  207 eq?
  153 call-with-values
  165 values
  336 apply
  384 cdr
  898 cons
  747 error
  555 void
  645 list
|#


;;; FIXME URGENT: should handle (+ x k), (- x k) where k is a fixnum
;;;               also fx+, fx-
(module (optimize-primcall)
  (define (optimize-primcall ctxt op rand*)
    (cond
      [(getprop op *cookie*) =>
       (lambda (proc)
         (proc ctxt op rand* 
               (lambda () 
                 (make-funcall (make-primref op) rand*))))]
      [else
       (make-funcall (make-primref op) rand*)]))
  (define (constant-value x k) 
    (struct-case x 
      [(constant t) (k t)] ; known
      [(bind lhs* rhs* body) (constant-value body k)]
      [(fix lhs* rhs* body) (constant-value body k)]
      [(seq e0 e1) (constant-value e1 k)]
      [else #f]))
  (define (mk-seq e0 e1)  ;;; keep e1 seq-free.
    (cond
      [(and (primcall? e0) (eq? (primcall-op e0) 'void)) e1]
      [(or (constant? e0) (primref? e0)) e1]
      [(seq? e1)
       (make-seq (make-seq e0 (seq-e0 e1)) (seq-e1 e1))]
      [else
       (make-seq e0 e1)]))
  (define (equable? x)
    (if (number? x) (fixnum? x) #t))
  (define *cookie* (gensym "optimizer-cookie"))
  (define-syntax set-cases
    (syntax-rules ()
      [(_ ctxt op rand* giveup 
          [(op** ...) b* b** ...] ...)
       (begin
         (let ([p (lambda (ctxt op rand* giveup) b* b** ...)])
           (putprop 'op** *cookie* p) ...
           (void)) ...)]))
  (set-cases ctxt op rand* giveup
    [(eq?)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (or
                  (constant-value a0
                    (lambda (x0)
                      (constant-value a1
                        (lambda (x1)
                          (mk-seq (mk-seq a0 a1)
                            (make-constant (eq? x0 x1)))))
                  (and (eq? ctxt 'e)
                       (mk-seq a0 a1)))))))
         (giveup))] 
    [(eqv?)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (or
                  (constant-value a0
                    (lambda (x0)
                      (or (constant-value a1
                            (lambda (x1)
                              (mk-seq (mk-seq a0 a1)
                                (make-constant (eqv? x0 x1)))))
                          (and (equable? x0)
                               (optimize-primcall ctxt 'eq? rand*)))))
                  (constant-value a1
                    (lambda (x1)
                      (and (equable? x1)
                           (optimize-primcall ctxt 'eq? rand*))))
                  (and (eq? ctxt 'e)
                       (mk-seq a0 a1)))))
         (giveup))]
    [(memv)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (constant-value a1
                  (lambda (ls)
                    (cond
                      [(not (list? ls)) #f]
                      [(eq? ctxt 'e) (mk-seq a0 a1)]
                      [(constant-value a0
                         (lambda (x)
                           (mk-seq (mk-seq a0 a1)
                             (case ctxt
                               [(v) (make-constant (memv x ls))]
                               [else (make-constant
                                       (if (memv x ls) #t #f))]))))]
                      [(andmap equable? ls)
                       (optimize-primcall ctxt 'memq rand*)]
                      [(fx= (length ls) 1)
                       (mk-seq a1
                         (optimize-primcall ctxt 'eqv?
                            (list a0 (make-constant (car ls)))))]
                      [else #f])))))
         (giveup))]
    [(memq) 
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (constant-value a1
                  (lambda (ls)
                    (cond
                      [(not (list? ls)) #f]
                      [(eq? ctxt 'e) (make-seq a0 a1)]
                      [(constant-value a0
                         (lambda (x)
                           (mk-seq (mk-seq a0 a1)
                             (case ctxt
                               [(v) (make-constant (memq x ls))]
                               [else (make-constant
                                       (if (memq x ls) #t #f))]))))]
                      [(fx= (length ls) 1)
                       (mk-seq a1
                         (optimize-primcall ctxt 'eq?
                           (list a0 (make-constant (car ls)))))]
                      [else (make-funcall (make-primref '$memq) rand*)])))))
         (giveup))]
    [(length) 
     (or (and (fx= (length rand*) 1)
              (let ([a0 (car rand*)])
                (constant-value a0
                  (lambda (ls)
                    (cond
                      [(not (list? ls)) #f]
                      [(eq? ctxt 'v) (make-constant (length ls))]
                      [(eq? ctxt 'e) a0]
                      [else (mk-seq a0 (make-constant #t))])))))
         (giveup))] 
    [(list vector)
     (case ctxt
       [(v) 
        (if (null? rand*) 
            (make-constant 
              (case op
                [(list)    '()]
                [else     '#()]))
            (giveup))]
       [else
        (if (null? rand*)
            (make-constant #t)
            (let f ([a (car rand*)] [d (cdr rand*)])
              (cond
                [(null? d) (mk-seq a (make-constant #t))]
                [else
                 (f (mk-seq a (car d)) (cdr d))])))])]
    [(cons*)
     (case ctxt
       [(e) 
        (cond
          [(null? rand*) (giveup)]
          [else
           (let f ([a (car rand*)] [d (cdr rand*)])
             (cond
               [(null? d) a]
               [else (f (mk-seq a (car d)) (cdr d))]))])]
       [(p) 
        (cond
          [(null? rand*) (giveup)]
          [(null? (cdr rand*)) 
           (let ([a (car rand*)])
             (or (constant-value a
                   (lambda (v)
                     (mk-seq a (make-constant (if v #t #f)))))
                 a))]
          [else 
           (let f ([a (car rand*)] [d (cdr rand*)])
             (cond
               [(null? d) (mk-seq a (make-constant #t))]
               [else (f (mk-seq a (car d)) (cdr d))]))])]
       [else
        (cond
          [(null? rand*) (giveup)]
          [(null? (cdr rand*)) (car rand*)]
          [else (giveup)])])] 
    [(cons)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (case ctxt
                  [(e) (mk-seq a0 a1)]
                  [(p) (mk-seq (mk-seq a0 a1) (make-constant #t))]
                  [else (giveup)])))
         (giveup))]
    [($struct-ref $struct/rtd?)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (case ctxt
                  [(e) (mk-seq a0 a1)]
                  [else 
                   (or (constant-value a1
                         (lambda (n1)
                           (mk-seq a1
                             (make-funcall (make-primref op)
                                (list a0 (make-constant n1))))))
                       (make-funcall (make-primref op) rand*))])))
         (error 'optimize "invalid operands to primitive"
            (map unparse rand*) op))]
    [(void)
     (or (and (null? rand*)
              (case ctxt
                [(p) (make-constant #t)]
                [else (make-constant (void))]))
         (giveup))]
    [(car cdr)
     (or (and (fx= (length rand*) 1)
              (let ([a (car rand*)])
                (constant-value a
                  (lambda (v)
                    (and (pair? v)
                         (mk-seq a
                           (make-constant
                             (case op
                               [(car) (car v)]
                               [else  (cdr v)]))))))))
         (giveup))]
    [(cadr)
     (or (and (fx= (length rand*) 1)
              (let ([a (car rand*)])
                (or (constant-value a
                      (lambda (v)
                        (and (pair? v)
                             (pair? (cdr v))
                             (mk-seq a
                               (make-constant
                                 (cadr v))))))
                    (make-funcall (make-primref op) rand*))))
         (giveup))] 
    [(not null? pair? fixnum? vector? string? char? symbol?
      eof-object?)
     (or (and (fx= (length rand*) 1)
              (let ([a (car rand*)])
                (case ctxt
                  [(e) a]
                  [else
                   (or (constant-value a
                         (lambda (v)
                           (mk-seq a
                             (make-constant
                               (case op
                                 [(not) (not v)]
                                 [(null?) (null? v)]
                                 [(pair?) (pair? v)]
                                 [(fixnum?) (fixnum? v)]
                                 [(vector?) (vector? v)]
                                 [(string?) (string? v)]
                                 [(char?) (char? v)]
                                 [(symbol?) (symbol? v)]
                                 [(eof-object?) (eof-object? v)]
                                 [else 
                                  (error 'optimize
                                    "huh ~s" op)])))))
                       (make-funcall (make-primref op) rand*))])))
         (giveup))]
    [($car $cdr)
     (or (and (fx= (length rand*) 1)
              (let ([a (car rand*)])
                (or (constant-value a
                      (lambda (v)
                        (if (pair? v)
                            (make-seq a
                              (make-constant
                                (case op
                                  [($car) (car v)]
                                  [else   (cdr v)])))
                            (error 'optimize
                                   "incorrect arg ~s to ~s"
                                   v op))))
                    (giveup))))
         (error 'optimize "incorrect args to primitive"
                (map unparse rand*) op))]
    [(fxadd1 fxsub1)
     (or (and (fx= (length rand*) 1)
              (let ([a (car rand*)])
                (or (constant-value a
                      (lambda (v)
                        (and (fixnum? v)
                             (let ([t 
                                    (case op
                                      [(fxadd1) (add1 v)]
                                      [else     (sub1 v)])])
                                (and (fixnum? t)
                                     (mk-seq a 
                                       (make-constant t)))))))
                    (make-funcall (make-primref op) rand*))))
         (giveup))]
    [(fx+)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (or (constant-value a1
                      (lambda (v1)
                        (and (fixnum? v1)
                             (or (constant-value a0
                                   (lambda (v0)
                                     (and (fixnum? v0)
                                          (let ([r (+ v0 v1)])
                                            (and (fixnum? r)
                                                 (mk-seq (mk-seq a0 a1) 
                                                   (make-constant r)))))))
                                 (mk-seq a1 
                                   (make-funcall (make-primref op) 
                                     (list a0 (make-constant v1))))))))
                    (constant-value a0
                      (lambda (v0)
                        (and (fixnum? v0)
                             (mk-seq a0 
                               (make-funcall (make-primref op)
                                  (list (make-constant v0) a1))))))
                    (make-funcall (make-primref op) rand*))))
         (giveup))]
    [(-)
     (or (and (>= (length rand*) 1)
              (andmap 
                (lambda (x) 
                  (constant-value x number?))
                rand*)
              (begin 
                (let ([r (apply - 
                           (map (lambda (x)
                                  (constant-value x 
                                    (lambda (v) v)))
                                rand*))])
                  (let f ([rand* rand*])
                    (cond
                      [(null? rand*) (make-constant r)]
                      [else
                       (mk-seq (car rand*) (f (cdr rand*)))])))))
         (giveup))]
    [(+ *)
     (or (and (>= (length rand*) 0)
              (andmap 
                (lambda (x) 
                  (constant-value x number?))
                rand*)
              (begin 
                (let ([r (apply 
                           (case op
                             [(+) +]
                             [(*) *]
                             [else (error 'ikarus "BUG: no prim" op)])
                           (map (lambda (x)
                                  (constant-value x 
                                    (lambda (v) v)))
                                rand*))])
                  (let f ([rand* rand*])
                    (cond
                      [(null? rand*) (make-constant r)]
                      [else
                       (mk-seq (car rand*) (f (cdr rand*)))])))))
         (giveup))]
    [(expt)
     (or (and (= (length rand*) 2)
              (andmap 
                (lambda (x) 
                  (constant-value x 
                    (lambda (v) (or (fixnum? v) (bignum? v)))))
                rand*)
              (begin 
                (let ([r (apply expt 
                           (map (lambda (x)
                                  (constant-value x 
                                    (lambda (v) v)))
                                rand*))])
                  (let f ([rand* rand*])
                    (cond
                      [(null? rand*) (make-constant r)]
                      [else
                       (mk-seq (car rand*) (f (cdr rand*)))])))))
         (giveup))]
    ;X; [(fx- fx+ fx*)
    ;X;  (or (and (fx= (length rand*) 2)
    ;X;           (let ([a0 (car rand*)] [a1 (cadr rand*)])
    ;X;             (or (constant-value a1
    ;X;                   (lambda (v1)
    ;X;                     (and (fixnum? v1)
    ;X;                          (or (constant-value a0
    ;X;                                (lambda (v0)
    ;X;                                  (and (fixnum? v0)
    ;X;                                       (let ([r (case op
    ;X;                                                  [(fx+) (+ v0 v1)]
    ;X;                                                  [(fx-) (- v0 v1)]
    ;X;                                                  [(fx*) (* v0 v1)]
    ;X;                                                  [else (error 'compile "BOO")])])
    ;X;                                         (and (fixnum? r)
    ;X;                                               (mk-seq (mk-seq a0 a1)
    ;X;                                                 (make-constant r)))))))
    ;X;                              (mk-seq a1 (make-primcall op (list a0 v1)))))))
    ;X;                 (constant-value a0
    ;X;                   (lambda (v0)
    ;X;                     (and (fixnum? v0)
    ;X;                          (mk-seq a0 (make-primcall op (list v0 a1))))))
    ;X;                 (make-primcall op (list a0 a1)))))
    ;X;      (giveup))]
    ;;; unoptimizables
    [(error syntax-error $syntax-dispatch $sc-put-cte 
      apply) 
     (giveup)]
    ))


(define (mk-mvcall p c)
  (struct-case p
    [(funcall) (make-mvcall p c)]
    [(seq e0 e1)
     (make-seq e0 (mk-mvcall e1 c))]
    [(bind lhs* rhs* body)
     (make-bind lhs* rhs* (mk-mvcall body c))]
    [else (error 'mk-mvcall "invalid producer" (unparse p))]))


(define (copy-propagate x)
  (define who 'copy-propagate)
  (define the-void (make-constant (void)))
  (define (known-value x) 
    (struct-case x 
      [(constant) x] ; known
      [(primref)  x] ; known
      [(bind lhs* rhs* body) (known-value body)]
      [(fix lhs* rhs* body) (known-value body)]
      [(seq e0 e1) (known-value e1)]
      [else #f]))
    
  (define (same-values? x y)
    (cond
      [(constant? x)
       (and (constant? y) 
            (eq? (constant-value x) 
                 (constant-value y)))]
      [(primref? x)
       (and (primref? y)
            (eq? (primref-name x)
                 (primref-name y)))]
      [else #f]))
  (define (predicate-value x)
    (struct-case x
      [(constant t) (if t 't 'f)]
      [(bind lhs rhs body) (predicate-value body)]
      [(fix lhs rhs body) (predicate-value body)]
      [(seq e0 e1) (predicate-value e1)]
      [else #f]))
  (define (do-conditional e0 e1 e2 k)
    (let ([e0 (Pred e0)])
      (cond
        [(predicate-value e0) =>
         (lambda (v)
           (if (eq? v 't) (k e1) (k e2)))]
        [else
         (make-conditional e0 (k e1) (k e2))])))
  (define (partition-referenced lhs* rhs*)
    (cond
      [(null? lhs*) (values '() '() the-void)]
      [else
       (let ([lhs (car lhs*)] [rhs (car rhs*)])
         (let-values ([(lhs* rhs* eff*) 
                       (partition-referenced
                           (cdr lhs*) (cdr rhs*))])
           (cond
             [(var-referenced lhs)
              (values (cons lhs lhs*) (cons rhs rhs*) eff*)]
             [else
              (values lhs* rhs* 
                (mk-seq eff* 
                  (Effect rhs)))])))]))
  (define (partition/assign-known lhs* rhs*)
    (cond
      [(null? lhs*) (values '() '() the-void)]
      [else
       (let ([lhs (car lhs*)] [rhs (car rhs*)])
         (let-values ([(lhs* rhs* eff*) 
                       (partition/assign-known
                           (cdr lhs*) (cdr rhs*))])
           (cond
             [(and (not (var-assigned lhs)) (known-value rhs)) =>
              (lambda (v)
                (set-var-referenced! lhs v)
                (values lhs* rhs* (mk-seq eff* rhs)))]
             [else
              (values (cons lhs lhs*) (cons rhs rhs*) eff*)])))]))
  (define (do-bind lhs* rhs* body k) 
    (let-values ([(lhs* rhs* eff0)
                  (partition-referenced lhs* rhs*)])
      (let ([rhs* (map Value rhs*)])
        (let-values ([(lhs* rhs* eff1)
                      (partition/assign-known lhs* rhs*)])
          (let ([body
                 (cond
                   [(null? lhs*) (k body)]
                   [else
                    (make-bind lhs* rhs* (k body))])])
            (mk-seq (mk-seq eff0 eff1) body))))))
  (define (do-fix lhs* rhs* body k) 
    (let-values ([(lhs* rhs* eff*) 
                  (partition-referenced lhs* rhs*)])
      (cond
        [(null? lhs*) (k body)]
        [else
         (make-fix lhs* (map Value rhs*) (k body))])))
  (define (mk-seq e0 e1)  ;;; keep e1 seq-free.
    (cond
      [(and (primcall? e0) (eq? (primcall-op e0) 'void)) e1]
      [(primref? e0) e1]
      [(seq? e1)
       (make-seq (make-seq e0 (seq-e0 e1)) (seq-e1 e1))]
      [else
       (make-seq e0 e1)]))
  (define (do-clambda g cls* cp free name)
    (make-clambda g
      (map (lambda (cls)
             (struct-case cls
               [(clambda-case info body)
                (make-clambda-case info (Value body))]))
           cls*)
      cp free name))
  (define (MKEffect ctxt)
    (define (Effect x)
      (struct-case x
        [(constant) the-void]
        [(var)      the-void]
        [(primref)  the-void]
        [(bind lhs* rhs* body)
         (do-bind lhs* rhs* body Effect)]
        [(fix lhs* rhs* body)
         (do-fix lhs* rhs* body Effect)]
        [(conditional e0 e1 e2)
         (let ([e0 (Pred e0)])
           (cond
             [(predicate-value e0) =>
              (lambda (v)
                (mk-seq e0 (if (eq? v 't) (Effect e1) (Effect e2))))]
             [else
              (make-conditional e0 (Effect e1) (Effect e2))]))]
        [(seq e0 e1) (mk-seq (Effect e0) (Effect e1))]
        [(clambda g cls*) the-void]
        [(primcall rator rand*) 
         (optimize-primcall ctxt rator (map Value rand*))]
        [(funcall rator rand*)
         (let ([rator (Value rator)])
           (cond
             [(known-value rator) =>
              (lambda (v)
                (struct-case v
                  [(primref op)
                   (mk-seq rator
                      (optimize-primcall ctxt op (map Value rand*)))]
                  [else
                   (make-funcall rator (map Value rand*))]))]
             [else (make-funcall rator (map Value rand*))]))]
        [(forcall rator rand*) 
         (make-forcall rator (map Value rand*))]
        [(mvcall p c)
         (mk-mvcall (Value p) (Value c))]
        [(assign lhs rhs)
         (unless (var-assigned lhs)
           (error who "var is not assigned" lhs))
         (if (var-referenced lhs)
             (make-assign lhs (Value rhs))
             (Effect rhs))]
        [else (error who "invalid effect expression" (unparse x))]))
    Effect)
  (define Effect (MKEffect 'e))
  (define (Pred x)
    (struct-case x
      [(constant) x]
      [(var) 
       (let ([r (var-referenced x)])
         (cond
           [(boolean? r) x]
           [else (Pred r)]))]
      [(primref) (make-constant #t)]
      [(bind lhs* rhs* body)
       (do-bind lhs* rhs* body Pred)]
      [(fix lhs* rhs* body)
       (do-fix lhs* rhs* body Pred)]
      [(conditional e0 e1 e2)