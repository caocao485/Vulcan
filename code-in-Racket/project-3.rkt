#lang racket
(define-struct var (name) #:transparent)
(define-struct proc (param body) #:transparent)
#;(define-struct let0 (var exp body) #:transparent)
(define-struct app (rator rand) #:transparent)
(define-struct opE (op left right) #:transparent)
(define-struct consE (first rest) #:transparent)
(define-struct firstE (cons) #:transparent)
(define-struct restE  (cons) #:transparent)
(define-struct ifE (test consq alt) #:transparent)
(define-struct letrecE (var exp body) #:transparent)
(define-struct rec-let (lhs   ; variable
                        rhs   ; required to be a lambda-expression
                        body)#:transparent)


;;Value
(define-struct boolV (b)#:transparent)
(define-struct const (number) #:transparent)
(define-struct closure (param body env) #:transparent)
(define-struct nullV ())
(define-struct consV (first rest) #:transparent)

(define (is-op? x) (not(not (member x (list '+ '- '* '/ '<= '>= '< '< '=)))))
(define (is-Boolean? x)(and (symbol? x)
                            (or (symbol=? x 'true)
                                (symbol=? x 'false))))
(define (is-null? x )(and (symbol? x)
                          (symbol=? x 'null)))
(define nullV0 (nullV))
(define (symbol->boolean x)
  (if(boolean? x)
     x
     (and (symbol? x)
          (or (symbol=? x 'true)
              (symbol=? x 'false)))))

(define parse
  (lambda(M)
    (match M
      [(? integer?)
       (const M)]
      [(? is-null?)
       nullV0]
      [(or (? boolean? b) (? is-Boolean? b)) 
       (boolV (symbol->boolean b))]
      [(? symbol?)
       (if(equal? M 'lambda)
          (error "lambda should not be identifier")
          (var M))]
      [`(,(? is-op? op) ,a ,b)
       (opE op (parse a)(parse b))]
      [`(lambda (,(? symbol? a)) ,b)
       (proc a (parse b))]
      [`(let ((,(? symbol? var0) ,exp)) ,body)
       (app  (proc var0 (parse body)) (parse exp))]
      [`(letrec ((,(? symbol? var0) ,exp)) ,body)
       (app  (proc var0 (parse body))
             (app (parse '(lambda(f) ;;Y combinator
                            ((lambda(x)
                               (f(lambda(y)((x x) y))))
                             (lambda(x)
                               (f(lambda(y)((x x) y)))))))
                  (proc var0 (parse exp))))]
      [`(cons ,firstE ,restE)
       (consE (parse firstE)(parse restE))]
      [`(first ,consE)
       (firstE (parse consE))]
      [`(rest ,consE)
       (restE (parse consE))]
      [`(if ,test ,consq ,alt)
       (ifE (parse test)(parse consq)(parse alt))]
      [`(,rator ,rand)
       (app (parse rator)(parse rand))]
      [else
       (error M "no match expression")])))

(module+ test
  (require rackunit)
  (check-equal? (const 1) (parse '1))
  (check-equal? (boolV #t)(parse 'true))
  (check-equal? (var 'a) (parse 'a))
  (check-equal? (proc   'x  (opE '+(var 'x) (const 1)))
                (parse '(lambda(x)(+ x 1))))
  (check-equal? (opE '+ (const 1)(const 2))
                (parse '(+ 1 2)))
  (check-equal? (app (proc  'x  (opE '+  (var 'x) (const 1)))
                     (const 2))
                (parse '((lambda(x)(+ x 1))2)))
  (check-equal? (consE (const 1)(const 2))
                (parse '(cons 1 2)))
  (check-equal? (consE (const 1) nullV0)
                (parse '(cons 1 null)))
  (check-equal? (firstE (consE (const 1) nullV0))
                (parse '(first (cons 1 null))))
  (check-equal? (ifE (boolV #t) (const 1)(const 2))
                (parse '(if #t 1 2)))
  (check-equal? (parse '(letrec ((x (lambda(i)(if(< i 1)0(+ i (x (- i 1)))))))(x 5)))
                (app
                 (proc 'x (app (var 'x) (const 5)))
                 (app
                  (proc
                   'f
                   (app
                    (proc 'x (app (var 'f) (proc 'y (app (app (var 'x) (var 'x)) (var 'y)))))
                    (proc 'x (app (var 'f) (proc 'y (app (app (var 'x) (var 'x)) (var 'y)))))))
                  (proc
                   'x
                   (proc
                    'i
                    (ifE
                     (opE '< (var 'i) (const 1))
                     (const 0)
                     (opE '+ (var 'i) (app (var 'x) (opE '- (var 'i) (const 1))))))))))
  (check-equal? (app (app
                      (proc   'y
                              (proc   'x  (opE '+ (var 'x) (var 'y))))
                      (const 2))
                     (const 1))
                (parse '(((lambda(y)(lambda(x)(+ x y)))2)1)))
  (check-equal? (app (proc 'x (opE '+ (var 'x) (const 1))) (const 1))
                (parse '(let ((x 1)) (+ x 1)))))


(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

;;op util function
(define (get-opV op-exp eval-op)
  (if(not(opE? op-exp))
     (error op-exp "is not a operat expression")
     (let([op (eval (opE-op op-exp) ns)]
          [leftV(eval-op (opE-left op-exp))]
          [rightV (eval-op (opE-right op-exp))])
       (if(or(not(const? leftV))(not(const? rightV)))
          (error op-exp "is not evaluated to number")
          (let([resultV (op (const-number leftV)
                            (const-number rightV))])
            (if(boolean? resultV)
               (boolV resultV)
               (const resultV)))))))


;; AST → V  AST ; an illegal program can return an AST
(define eval0
  (lambda (M) ; M is an AST
    (cond ; case split on form of M
      ((var? M) (error "free var")) ; M is a free var (stuck!)
      ((or (const? M) (proc? M)(nullV? M)(boolV? M)) M) ; M is a value
      ((opE? M) ; M has form (+ l r)
       (get-opV M eval0))
      ((consE? M) 
       (consV  (eval0 (consE-first M))
               (eval0 (consE-rest M))))
      ((firstE? M)
       (let([cons-result (eval0 (firstE-cons M))])
         (if(not(consV? cons-result))
            (error M "first cannot operate this result")
            (consV-first cons-result))))
      ((restE? M)
       (let([cons-result (eval0 (firstE-cons M))])
         (if(not(consV? cons-result))
            (error M "rest cannot operate this result")
            (consV-rest cons-result))))
      ((ifE? M)
       (let([test-result (eval0 (ifE-test M))])
         (if(not(boolV test-result))
            (error M "test result are not booleanV")
            (if(boolV-b test-result)
               (eval0 (ifE-consq M))
               (eval0 (ifE-alt M))))))
      (else ; M has form (N1 N2)
       (apply (eval0 (app-rator M)) (eval0 (app-rand M)))))))
;; A═►B A → B
(define apply (lambda (a-proc a-value)
                (cond
                  ((not (proc? a-proc)) ; ill-formed app
                   (error "not a app")) ; return stuck state
                  (else ; return reduced,substituted body
                   (eval0
                    (subst a-value (proc-param a-proc)(proc-body a-proc)))))))

;; V Sym R → R Blindly substitutes v for x in M (ignoring capture)
(define subst
  (lambda (v x M)
    (cond
      [(var? M) (cond [(equal? (var-name M) x) v] [else M])]
      [(const? M) M]
      [(proc? M)
       (cond [(equal? x (proc-param M)) M]
             [else (make-proc (proc-param M)
                              (subst v x (proc-body M)))])]
      [(opE? M) (make-opE (opE-op M)
                          (subst v x (opE-left M))
                          (subst v x (opE-right M)))]
      [(consE? M)
       (make-consE (subst v x (consE-first M))
                   (subst v x (consE-rest M)))]
      [(firstE? M)
       (make-firstE (subst v x (firstE-cons M)))]
      [(restE? M)
       (make-restE (subst v x (restE-cons M)))]
      [(ifE? M)
       (make-ifE (subst v x (ifE-test M))
                 (subst v x (ifE-consq M))
                 (subst v x (ifE-alt M)))]
      [else ;; M is (N1 N2)
       (make-app (subst v x (app-rator M))
                 (subst v x (app-rand M)))])))

(module+ test
  (require rackunit)
  (check-equal? (eval0 (parse '(+ 1 2)))
                (const 3))
  (check-equal? (eval0 (parse '1))
                (const 1))
  (check-equal? (eval0 (parse '(lambda(x)x)))
                (proc 'x (var 'x)))
  (check-equal? (eval0 (parse '((lambda(x)(+ x 1))3)))
                (const 4))
  (check-equal? (eval0 (parse '(((lambda(x)(lambda(y)(+ x y)))1)2)))
                (const 3))
  (check-equal? (eval0 (parse '(((lambda(x)(lambda(y)(+ x x)))1)2)))
                (const 2))
  (check-equal? (eval0 (parse '(((lambda(x)(lambda(y)(* ((lambda(x)x) 2) x)))1)2)))
                (const 2))
  (check-equal? (eval0 (parse '(((lambda(x)(lambda(y)(<= ((lambda(x)x) 2) x)))1)2)))
                (boolV #f))
  (check-equal? (eval0 (parse '(((lambda(x)(lambda(y)(cons ((lambda(x)x) 2) x)))1)2)))
                (consV (const 2) (const 1)))
  (check-equal? (eval0 (app (proc 'x (opE '+ (var 'x) (const 1))) (const 1)))
                (eval0 (parse '(let ((x 1)) (+ x 1)))))
  (check-equal? (eval0(consE (const 1)(const 2)))
                (eval0(parse '(cons 1 2))))
  (check-equal? (eval0(consE (const 1) nullV0))
                (eval0(parse '(cons 1 null))))
  (check-equal? (eval0(firstE (consE (const 1) nullV0)))
                (eval0(parse '(first (cons 1 null)))))
  (check-equal? (eval0(ifE (boolV #t) (const 1)(const 2)))
                (eval0(parse '(if #t 1 2))))
  (check-equal? (eval0 (parse '(letrec ((sum (lambda(i)(if(< i 1)0(+ i (sum (- i 1)))))))(sum 5))))
                (eval0 (parse '15)))
  (check-exn exn:fail? (lambda()(eval0 (parse '(((lambda(x)(lambda(y)(+ ((lambda(x)z) 2) x)))1)2)))
                         ))
  (check-exn exn:fail?  (lambda()(eval0 (parse '((lambda (x) (+ x y)) 7)))
                          ))
  (check-exn exn:fail? (lambda()(eval0 (parse '((lambda (x) (1 x)) 7)))
                         )))

;; constructor for static distance (sd) abstract syntax
(define-struct sdvar (index)#:transparent)
(define-struct sdproc (body)#:transparent)
;; SD-Expr ::= (make-var I) | (make-const N) | (make-sdproc SD-Expr) | (make-app SD-Expr SD-Expr)
;; where I is the set of positive numbers and N is the set of numbers


(define sd
  (lambda (an-ar binding-vars)
    (cond
      ((var? an-ar) (make-sdvar (sdlookup (var-name an-ar) binding-vars)))
      ((const? an-ar) an-ar)
      ((proc? an-ar) 
       (make-sdproc 
        (sd (proc-body an-ar) (cons (proc-param an-ar) binding-vars))))
      ((opE? an-ar)
       (opE (opE-op an-ar)
            (sd (opE-left an-ar) binding-vars)
            (sd (opE-right an-ar) binding-vars)))
      ((consE? an-ar)
       (consE (sd (consE-first an-ar) binding-vars)
              (sd (consE-rest an-ar) binding-vars)))
      ((firstE? an-ar)
       (firstE (sd(firstE-cons an-ar) binding-vars)))
      ((restE? an-ar)
       (restE (sd (restE-cons an-ar) binding-vars)))
      ((ifE? an-ar)
       (ifE (sd (ifE-test an-ar) binding-vars)
            (sd (ifE-consq an-ar) binding-vars)
            (sd (ifE-alt an-ar) binding-vars)))
      (else
       (make-app (sd (app-rator an-ar) binding-vars) 
                 (sd (app-rand an-ar) binding-vars))))))


(define sdlookup
  (lambda (a-var vars)
    (cond
      ((null? vars) (error 'sdlookup "free occurrence of ~s" a-var))
      (else (if (eq? (car vars) a-var)
                1
                (add1 (sdlookup a-var (cdr vars))))))))


(module+ test
  (require rackunit)
  (check-equal?
   (sd
    (proc 'z
          (proc 'x
                (app
                 (proc 'z
                       (app (var 'z)
                            (app (var 'z)
                                 (app (var 'z) (var 'x)))))
                 (var 'x)))) null)
   (sdproc
    (sdproc
     (app
      (sdproc (app (sdvar 1) (app (sdvar 1) (app (sdvar 1) (sdvar 2)))))
      (sdvar 1)))))
  (check-equal?
   (sd (parse '(lambda(x)x)) null)
   (sdproc
    (sdvar 1)))
  (check-equal?
   (sd (parse '(lambda(x)(+ x 1))) null)
   (sdproc
    (opE '+ (sdvar 1) (const 1))))
  (check-equal?
   (sd (parse '(let ((x 1)) (+ x 1))) null)
   (sd (app (proc 'x (opE '+ (var 'x) (const 1))) (const 1)) null))
  (check-equal?
   (sd (parse '(lambda(z)(lambda(x)((lambda(z) (if (z(z(z x))) z z))x)))) null)
   (sdproc
    (sdproc
     (app
      (sdproc
       (ifE
        (app (sdvar 1) (app (sdvar 1) (app (sdvar 1) (sdvar 2))))
        (sdvar 1)
        (sdvar 1)))
      (sdvar 1))))))
   

#;(sd (proc  'z
             (proc  'x (app
                        (proc 'z
                              (app (var 'z)
                                   (app (var 'z)
                                        (app (var 'z) (var 'x)))))
                        (var 'x)))) null)

#;(eval0 (app(app (proc  'z
                         (proc  'x (app
                                    (proc 'z
                                          (app (var 'z)
                                               (app (var 'z)
                                                    (app (var 'z) (var 'x)))))
                                    (var 'x))))1) 1) )

;;op util function
(define (get-opV1 op-exp eval-op env)
  (if(not(opE? op-exp))
     (error op-exp "is not a operat expression")
     (let([op (eval (opE-op op-exp) ns)]
          [leftV(eval-op (opE-left op-exp) env)]
          [rightV (eval-op (opE-right op-exp) env)])
       (if(or(not(const? leftV))(not(const? rightV)))
          (error op-exp "is not evaluated to number")
          (let([resultV (op (const-number leftV)
                            (const-number rightV))])
            (if(boolean? resultV)
               (boolV resultV)
               (const resultV)))))))


(define eval-env
  (lambda(M env)
    (match M
      [(? const?)
       M]
      [(? var?)
       (lookup M env)]
      [(boolV b)
       M]
      [(nullV)
       M]
      [(opE op left right)
       (get-opV1 M  eval-env env)]
      [(proc param body)
       (closure param body env)]
      [(consE first rest)
       (consV (eval-env first env)
              (eval-env rest env))]
      [(firstE consExp)
       (let([cons-result (eval-env consExp env)])
         (if(not(consV? cons-result))
            (error M "first cannot operate this result")
            (consV-first cons-result)))]
      [(restE consExp)
       (let([cons-result (eval-env consExp env)])
         (if(not(consV? cons-result))
            (error M "rest cannot operate this result")
            (consV-rest cons-result)))]
      [(ifE test consq alt)
       (let([test-result (eval-env test env)])
         (if(not(boolV test-result))
            (error M "test result are not booleanV")
            (if(boolV-b test-result)
               (eval-env consq env)
               (eval-env alt env))))]
      [(app rator rand)
       (match-define (closure param body env0) (eval-env rator env))
       (eval-env  body (extend param (eval-env rand env) env0))]
      [else
       (error M "cannot eval this expression")])))

(define (extend var value env)
  (cons (cons var value) env))
         
(define(lookup var env)
  (if(null? env)
     (error var "free var")
     (let([kv (first env)])
       (if(eq?(car kv) (var-name var))
          (cdr kv)
          (lookup var (rest env))))))
(define empty-env null)

(define rec-extend
  (lambda (M env)
    (local 
       [(define var (rec-let-lhs M))
        (define rhs (rec-let-rhs M))
	(define renv (extend env var (void)))]
      ; (void) is a dummy value for var
      ; Is renv the appropriate environment yet?  No.
      (set-first-pair-val! renv (eval-env rhs renv)))))

(define set-first-pair-val!
  (lambda (env val)
     (local
        [(define first-pair (first env))]
       (set-pair-val! first-pair val))))
(define (set-pair-val! pair val)
  (set! pair (cons (car pair)val)))


(module+ test
  (require rackunit)
  (check-equal? (eval-env (parse '(+ 1 2)) empty-env)
                (const 3))
  (check-equal? (eval-env (parse '1)  empty-env)
                (const 1))
  (check-equal? (eval-env (parse '(lambda(x)x)) empty-env)
                (closure 'x (var 'x) '()))
  (check-equal? (eval-env (parse '((lambda(x)(+ x 1))3)) empty-env)
                (const 4))
  (check-equal? (eval-env (parse '(((lambda(x)(lambda(y)(+ x y)))1)2)) empty-env)
                (const 3))
  (check-equal? (eval-env (parse '(((lambda(x)(lambda(y)(+ x x)))1)2)) empty-env)
                (const 2))
  (check-equal? (eval-env (parse '(((lambda(x)(lambda(y)(+ ((lambda(x)x) 2) x)))1)2)) empty-env)
                (const 3))
  (check-equal? (eval-env (app (proc 'x (opE '+ (var 'x) (const 1))) (const 1))empty-env)
                (eval-env (parse '(let ((x 1)) (+ x 1))) empty-env))
  (check-equal? (eval-env (parse '(((lambda(x)(lambda(y)(<= ((lambda(x)x) 2) x)))1)2))empty-env)
                (boolV #f))
  (check-equal? (eval-env (parse '(((lambda(x)(lambda(y)(cons ((lambda(x)x) 2) x)))1)2))empty-env)
                (consV (const 2) (const 1)))
  (check-equal? (eval-env (app (proc 'x (opE '+ (var 'x) (const 1))) (const 1))empty-env)
                (eval-env (parse '(let ((x 1)) (+ x 1)))empty-env))
  (check-equal? (eval-env(consE (const 1)(const 2))empty-env)
                (eval-env(parse '(cons 1 2))empty-env))
  (check-equal? (eval-env(consE (const 1) nullV0)empty-env)
                (eval-env(parse '(cons 1 null))empty-env))
  (check-equal? (eval-env(firstE (consE (const 1) nullV0))empty-env)
                (eval-env(parse '(first (cons 1 null)))empty-env))
  (check-equal? (eval-env(ifE (boolV #t) (const 1)(const 2))empty-env)
                (eval-env(parse '(if #t 1 2))empty-env))
  (check-equal? (eval-env (parse '(letrec ((sum (lambda(i)(if(< i 1)0(+ i (sum (- i 1)))))))(sum 5)))empty-env)
                (eval-env (parse '15)empty-env))
  (check-exn exn:fail? (lambda()
                         (eval-env (parse '(((lambda(x)(lambda(y)(+ ((lambda(x)z) 2) x)))1)2)) empty-env)
                         ))
  (check-exn exn:fail? (lambda()(eval-env (parse '((lambda (x) (+ x y)) 7)) empty-env)
                         ))
  (check-exn exn:fail? (lambda()(eval-env (parse '((lambda (x) (1 x)) 7)) empty-env)
                         )))



(define-struct thunk (exp (val #:mutable)(evaled? #:mutable))#:transparent)

(define (force-eval thunk0)
  (if(thunk? thunk0)
     (if(thunk-evaled? thunk0)
        (thunk-val thunk0)
        (begin
          (set-thunk-val! thunk0 (force-eval((thunk-exp thunk0))))
          (set-thunk-evaled?! thunk0 #t)
          (thunk-val thunk0)))
     thunk0))

;;op util function
(define (get-opV2 op-exp eval-op env)
  (if(not(opE? op-exp))
     (error op-exp "is not a operat expression")
     (let([op (eval (opE-op op-exp) ns)]
          [leftV (force-eval(eval-op (opE-left op-exp) env))]
          [rightV (force-eval(eval-op (opE-right op-exp) env))])
       (if(or(not(const? leftV))(not(const? rightV)))
          (error op-exp "is not evaluated to number")
          (let([resultV (op (const-number leftV)
                            (const-number rightV))])
            (if(boolean? resultV)
               (boolV resultV)
               (const resultV)))))))


(define eval-need
  (lambda(M env)
    (match M
      [(? const?)
       M]
      [(? var?)(lookup M env)]
      [(boolV b)
       M]
      [(nullV)
       M]
      [(opE op left right)
       (get-opV2 M  eval-need env)]
      [(consE first rest)
       (consV (force-eval(eval-need first env))
              (force-eval(eval-need rest env)))]
      [(firstE consExp)
       (let([cons-result (force-eval(eval-need consExp env))])
         (if(not(consV? cons-result))
            (error M "first cannot operate this result")
            (force-eval(consV-first cons-result))))]
      [(restE consExp)
       (let([cons-result (force-eval(eval-need consExp env))])
         (if(not(consV? cons-result))
            (error M "rest cannot operate this result")
            (force-eval(consV-rest cons-result))))]
      [(ifE test consq alt)
       (let([test-result (force-eval(eval-need test env))])
         (if(not(boolV test-result))
            (error M "test result are not booleanV")
            (if(boolV-b test-result)
               (force-eval(eval-need consq env))
               (force-eval(eval-need alt env)))))]
      [(proc param body)
       (closure param body env)]
      [(app rator rand)
       (match-define
         (closure param body env0)
         (force-eval(eval-need rator env)))
       (eval-need
        body
        (extend
         param
         (make-thunk (lambda()
                       (force-eval(eval-need rand env)))null #f) env0))]
      [else
       (error M "cannot eval this expression")])))


(module+ test
  (require rackunit)
  
  (define parse1
    (lambda(M)
      (match M
        [(? integer?)
         (const M)]
        [(? is-null?)
         nullV0]
        [(or (? boolean? b) (? is-Boolean? b)) 
         (boolV (symbol->boolean b))]
        [(? symbol?)
         (if(equal? M 'lambda)
            (error "lambda should not be identifier")
            (var M))]
        [`(,(? is-op? op) ,a ,b)
         (opE op (parse a)(parse b))]
        [`(lambda (,(? symbol? a)) ,b)
         (proc a (parse b))]
        [`(let ((,(? symbol? var0) ,exp)) ,body)
         (app  (proc var0 (parse body)) (parse exp))]
        [`(letrec ((,(? symbol? var0) ,exp)) ,body)
         (app  (proc var0 (parse body))
               (app (parse '(lambda(f) ;;Y combinator
                              ((lambda(x)
                                 (f(x x)))
                               (lambda(x)
                                 (f(x x))))))
                    (proc var0 (parse exp))))]
        [`(cons ,firstE ,restE)
         (consE (parse firstE)(parse restE))]
        [`(first ,consE)
         (firstE (parse consE))]
        [`(rest ,consE)
         (restE (parse consE))]
        [`(if ,test ,consq ,alt)
         (ifE (parse test)(parse consq)(parse alt))]
        [`(,rator ,rand)
         (app (parse rator)(parse rand))]
        [else
         (error M "no match expression")])))

  (check-equal? (force-eval (eval-need (parse '(+ 1 2))empty-env)) 
                (const 3))
  (check-equal? (force-eval(eval-need (parse '1)  empty-env)) 
                (const 1))
  (check-equal? (force-eval(eval-need (parse '(lambda(x)x)) empty-env)) 
                (closure 'x (var 'x) '()))
  (check-equal? (force-eval(eval-need (parse '(+ 1 ((lambda(x)(+ x 1))3))) empty-env)) 
                (const 5))
  (check-equal? (force-eval(eval-need (parse '(+ 1 (((lambda(x)(lambda(y)(+ x y)))1)2))) empty-env)) 
                (const 4))
  (check-equal? (force-eval(eval-need (app (proc 'x (opE '+ (var 'x) (const 1))) (const 2))empty-env))
                (eval-need (parse '(+ 1(let ((x 1)) (+ x 1)))) empty-env))
  (check-exn exn:fail? (lambda()
                         (force-eval(eval-need (parse '(((lambda(x)(lambda(y)(+ ((lambda(x)z) 2) x)))1)2)) empty-env))
                         ))
  (check-exn exn:fail? (lambda()
                         (force-eval(eval-need (parse '((lambda (x) (+ x y)) 7)) empty-env))
                         ))
  (check-exn exn:fail? (lambda()(force-eval(eval-need (parse '((lambda (x) (1 x)) 7)) empty-env))
                         ))
  (check-equal? (force-eval(eval-need (parse '(((lambda(y)(lambda(x)x))(+ 1 2))(+ 2 1))) null))
                (const 3))
  (check-equal? (force-eval(eval-need (parse '(((lambda(y)(lambda(x)(y x)))
                                                (lambda(x)(+ 2 x)))(+ 1 2))) null))
                (const 5))
  (check-equal? (force-eval(eval-need (parse '(((lambda(x)(lambda(y)(<= ((lambda(x)x) 2) x)))1)2))empty-env))
                (boolV #f))
  (check-equal? (force-eval(eval-need (parse '(((lambda(x)(lambda(y)(cons ((lambda(x)x) 2) x)))1)2))empty-env))
                (consV (const 2) (const 1)))
  (check-equal? (force-eval(eval-need(app (proc 'x (opE '+ (var 'x) (const 1))) (const 1))empty-env))
                (force-eval(eval-need (parse '(let ((x 1)) (+ x 1)))empty-env)))
  (check-equal? (force-eval(eval-need(consE (const 1)(const 2))empty-env))
                (force-eval(eval-need(parse '(cons 1 2))empty-env)))
  (check-equal? (force-eval(eval-need(consE (const 1) nullV0)empty-env))
                (force-eval(eval-need(parse '(cons 1 null))empty-env)))
  (check-equal? (force-eval(eval-need(firstE (consE (const 1) nullV0))empty-env))
                (force-eval(eval-need(parse '(first (cons 1 null)))empty-env)))
  (check-equal? (force-eval(eval-need(ifE (boolV #t) (const 1)(const 2))empty-env))
                (force-eval(eval-need(parse '(if #t 1 2))empty-env)))
  (check-equal? (force-eval(eval-need(parse '(letrec ((sum (lambda(i)(if(< i 1)0(+ i (sum (- i 1)))))))(sum 5)))empty-env))
                (force-eval(eval-need (parse '15)empty-env)))
  (check-equal? (force-eval(eval-need(parse1 '(letrec ((sum (lambda(i)(if(< i 1)0(+ i (sum (- i 1)))))))(sum 5)))empty-env))
                (force-eval(eval-need (parse '15)empty-env)))
  (check-exn exn:fail? (lambda()(force-eval (eval-need (parse '(
                                                                (
                                                                 (lambda(f)(f(lambda(x)x)))
                                                                 (lambda(xx)(lambda(zz)(xx zz )))
                                                                 )
                                                                (+ z 2)
                                                                ))null))
                         ))
  ;;same without force-eval
  (check-pred thunk?
              (eval-need (parse '(
                                  (
                                   (lambda(f)(f(lambda(x)x)))
                                   (lambda(xx)(lambda(zz)(xx zz )))
                                   )
                                  (+ z 2) ;;z: undefined;
                                  ))null)
              ))

(force-eval
 (eval-need
  (parse
   '(+ 1
       (let([a 5])
         (letrec ([app-to-a (lambda (f) (f a))])
           (let ([a 10]) (+ a (app-to-a (lambda (x) x))))))))null))


(+ 1 ;;16
   (let([a 5])
     (let ([app-to-a (lambda (f) (f a))])
       (let ([a 10]) (+ a (app-to-a (lambda (x) x)))))))

#;(
   (
    (lambda(f)(f(lambda(x)x)))
    (lambda(xx)(lambda(zz)(xx zz )))
    )
   (+ 1 2)
   )
#;(eval-need (parse '(let ((y 17))
                       (let ((f (lambda (x) (+ y y))))
                         (let ((y 2))
                           (f 0))))) null)
#;(eval-env (parse '(let ((y 17))
                      (let ((f (lambda (x) (+ y y))))
                        (let ((y 2))
                          (f 0))))) null)

;;Y'
#;((lambda(x)
     (lambda(y)
       ((x y) x)))
   (lambda(y)
     (lambda(x)
       (y ((x y) x)))))

(lambda(f)
  (lambda(z)
   ((lambda(x)
      (f(x x)))
    (lambda(x)
      (f(x x))))z))

(define-struct variable (n) #:transparent)
(define-struct ap (var number) #:transparent)
(define-struct lm (f body) #:transparent)

(define (build n)
  (if(= n 0)
     (variable 'x)
     (ap (variable 'f)(build (- n 1)))))

(define (writer n)
  (lm 'f (lm  'x (build n))))


(define (reader-step e)
  (match e
    [`(lm ,a ,b)
     (+ 1 (reader-step b))]
    [else
     0]))