#lang racket
(define-struct var (name) #:transparent)
(define-struct proc (param body) #:transparent)
#;(define-struct let0 (var exp body) #:transparent)
(define-struct app (rator rand) #:transparent)
(define-struct add (left right) #:transparent)

;;Value
(define-struct Boolean (b))
(define-struct const (number) #:transparent)
(define-struct closure (param body env) #:transparent)

(define parse
  (lambda(M)
    (match M
      [(? integer?)
       (const M)]
      [(? symbol?)
       (if(equal? M 'lambda)
          (error "lambda should not be identifier")
       (var M))]
      [`(+ ,a ,b)
       (add (parse a)(parse b))]
      [`(lambda (,(? symbol? a)) ,b)
       (proc a (parse b))]
      [`(let ((,(? symbol? var0) ,exp)) ,body)
       (app  (proc var0 (parse body)) (parse exp))]
      [`(,rator ,rand)
       (app (parse rator)(parse rand))]
      [else
       (error M "no match expression")])))

(module+ test
  (require rackunit)
  (check-equal? (const 1) (parse '1))
  (check-equal? (var 'a) (parse 'a))
  (check-equal? (proc   'x  (add (var 'x) (const 1)))
                (parse '(lambda(x)(+ x 1))))
  (check-equal? (add (const 1)(const 2))
                (parse '(+ 1 2)))
  (check-equal? (app (proc  'x  (add (var 'x) (const 1)))
                     (const 2))
                (parse '((lambda(x)(+ x 1))2)))
  (check-equal? (app (app
                      (proc   'y
                              (proc   'x  (add (var 'x) (var 'y))))
                      (const 2))
                     (const 1))
                (parse '(((lambda(y)(lambda(x)(+ x y)))2)1)))
  (check-equal? (app (proc 'x (add (var 'x) (const 1))) (const 1))
                (parse '(let ((x 1)) (+ x 1)))))
      
(define (const-add leftValue rightValue)
  (const (+ (const-number leftValue)
            (const-number rightValue))))
     
;; AST → V  AST ; an illegal program can return an AST
(define eval
  (lambda (M) ; M is an AST
    (cond ; case split on form of M
      ((var? M) (error "free var")) ; M is a free var (stuck!)
      ((or (const? M) (proc? M)) M) ; M is a value
      ((add? M) ; M has form (+ l r)
       (const-add (eval (add-left M)) (eval (add-right M))))
      (else ; M has form (N1 N2)
       (apply (eval (app-rator M)) (eval (app-rand M)))))))
;; A═►B A → B
(define apply (lambda (a-proc a-value)
                (cond
                  ((not (proc? a-proc)) ; ill-formed app
                   (error "not a app")) ; return stuck state
                  (else ; return reduced,substituted body
                   (eval
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
      [(add? M) (make-add (subst v x (add-left M))
                          (subst v x (add-right M)))]
      [else ;; M is (N1 N2)
       (make-app (subst v x (app-rator M))
                 (subst v x (app-rand M)))])))


(module+ test
  (require rackunit)
  (check-equal? (eval (parse '(+ 1 2)))
                (const 3))
  (check-equal? (eval (parse '1))
                (const 1))
  (check-equal? (eval (parse '(lambda(x)x)))
                (proc 'x (var 'x)))
  (check-equal? (eval (parse '((lambda(x)(+ x 1))3)))
                (const 4))
  (check-equal? (eval (parse '(((lambda(x)(lambda(y)(+ x y)))1)2)))
                (const 3))
  (check-equal? (eval (parse '(((lambda(x)(lambda(y)(+ x x)))1)2)))
                (const 2))
  (check-equal? (eval (parse '(((lambda(x)(lambda(y)(+ ((lambda(x)x) 2) x)))1)2)))
                (const 3))
  (check-equal? (eval (app (proc 'x (add (var 'x) (const 1))) (const 1)))
                (eval (parse '(let ((x 1)) (+ x 1)))))
  (check-exn exn:fail? (lambda()(eval (parse '(((lambda(x)(lambda(y)(+ ((lambda(x)z) 2) x)))1)2)))
                         ))
  (check-exn exn:fail?  (lambda()(eval (parse '((lambda (x) (+ x y)) 7)))
                          ))
  (check-exn exn:fail? (lambda()(eval (parse '((lambda (x) (1 x)) 7)))
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
      ((add? an-ar)
       (add (sd (add-left an-ar) binding-vars)
            (sd (add-right an-ar) binding-vars)))
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
    (add (sdvar 1) (const 1))))
  (check-equal?
   (sd (parse '(let ((x 1)) (+ x 1))) null)
   (sd (app (proc 'x (add (var 'x) (const 1))) (const 1)) null))
  (check-equal?
   (sd (parse '(lambda(z)(lambda(x)((lambda(z) (z(z(z x))))x)))) null)
   (sdproc
    (sdproc
     (app (sdproc (app (sdvar 1) (app (sdvar 1) (app (sdvar 1) (sdvar 2))))) (sdvar 1))))))
   

#;(sd (proc  'z
             (proc  'x (app
                        (proc 'z
                              (app (var 'z)
                                   (app (var 'z)
                                        (app (var 'z) (var 'x)))))
                        (var 'x)))) null)

#;(eval (app(app (proc  'z
                        (proc  'x (app
                                   (proc 'z
                                         (app (var 'z)
                                              (app (var 'z)
                                                   (app (var 'z) (var 'x)))))
                                   (var 'x))))1) 1) )

(define eval-env
  (lambda(M env)
    (match M
      [(? const?)
       M]
      [(? var?)
       (lookup M env)]
      [(add left right)
       (const-add (eval-env left env)
                  (eval-env right env))]
      [(proc param body)
       (closure param body env)]
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
  (check-equal? (eval-env (app (proc 'x (add (var 'x) (const 1))) (const 1))empty-env)
                (eval-env (parse '(let ((x 1)) (+ x 1))) empty-env))
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


(define eval-need
  (lambda(M env)
    (match M
      [(? const?)
       M]
      [(? var?)(lookup M env)]
      [(add left right)
       (const-add (force-eval(eval-need left env))
                  (force-eval(eval-need right env)))]
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
                       (eval-need rand env))null #f) env0))]
      [else
       (error M "cannot eval this expression")])))


(module+ test
  (require rackunit)
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
  (check-equal? (force-eval(eval-need (app (proc 'x (add (var 'x) (const 1))) (const 2))empty-env))
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
         (let ([app-to-a (lambda (f) (f a))])
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