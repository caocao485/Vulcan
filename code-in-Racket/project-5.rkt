#lang racket
;; Revised definition of the proc constructor required by Typed LC
(define-struct var (name) #:transparent)
(define-struct proc (param type body) #:transparent)
(define-struct boolV (bool) #:transparent)
(define-struct const (num) #:transparent)
(define-struct add (left right) #:transparent)
(define-struct if0 (tst  thn els) #:transparent)
(define-struct app (rator rand) #:transparent)
(define-struct rec (var type rhs body) #:transparent)
(define-struct setter (lhs rhs) #:transparent)

(define-struct nullV () #:transparent)

(define-struct closV (arg body env)#:transparent)


;; Definition of the type constructor for function (proc) types  
(define-struct -> (domain range) #:transparent)
(define-struct numT ())
(define NumT (numT))
(define-struct boolT ())
(define BoolT (boolT))


;; type ::= 'int  | (make-> type type)

;; typeEnv ::= '() | (cons (make-pair Sym type) typeEnv)
;; The struct named pair has previously been defined in our meta-interpreter for dynamically typed LC
(define (extend tenv tvar tvalue )
  (cons (cons tvar tvalue) tenv))
         
(define(lookup tvar tenv error)
  (if(null? tenv)
     (error)
     (let([kv (first tenv)])
       (if(eq?(car kv)  tvar)
          (cdr kv)
          (lookup tvar (rest tenv) error)))))
(define empty-tenv null)

(define (parse-arrow aw)
  (match aw
    ['int
     NumT]
    ['bool
     BoolT]
    [`(,domain -> ,range)
     (make--> (parse-arrow domain)
              (parse-arrow range))]))

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
      [(or (? boolean? b) (? is-Boolean? b)) 
       (boolV (symbol->boolean b))]
      [(? symbol?)
       (if(eq? M 'lambda)
          (error M "lambda should not be a identifier")
          (var M))]
      [`(+ ,a ,b)
       (add (parse a)(parse b))]
      [`(set! ,x ,rhs)
       (setter (parse x) (parse rhs))]
      [`(lambda (,(? symbol? a) : ,ptype)  ,b)
       (proc a (parse-arrow ptype)
             (parse b))]
      [`(if ,tst ,thn ,els)
       (if0 (parse tst) (parse thn) (parse els))]
      [`(let ((,(? symbol? var0) : ,tvar ,exp)) ,body)
       (rec var0 (parse-arrow tvar) (parse exp) (parse body))]
      [`(,rator ,rand)
       (app (parse rator)(parse rand))]
      [else
       (error M "no match expression")])))

(module+ test
  (require rackunit)
  (check-equal? (const 1) (parse '1))
  (check-equal? (var 'a) (parse 'a))
  (check-equal? (proc   'x NumT (add (var 'x) (const 1)))
                (parse '(lambda(x : int)(+ x 1))))
  (check-equal? (add (const 1)(const 2))
                (parse '(+ 1 2)))
  (check-equal? (app (proc  'x NumT (add (var 'x) (const 1)))
                     (const 2))
                (parse '((lambda(x : int)(+ x 1))2)))
  (check-equal? (app (app
                      (proc   'y NumT
                              (proc   'x NumT (add (var 'x) (var 'y))))
                      (const 2))
                     (const 1))
                (parse '(((lambda(y : int)(lambda(x : int)(+ x y)))2)1)))
  (check-equal? (rec 'x NumT (const 1) (add (var 'x) (const 1)))
                (parse '(let ((x : int 1)) (+ x 1))))
  (check-equal? (if0 (const 1) (const 2) (const 3))
                (parse '(if 1 2 3)))
  (check-equal? (setter (var 'x) (const 3))
                (parse '(set! x 3)))
  )

;; Type check a closed abstract representation of an LC expression
(define TypeCheck
  (lambda (exp)
    (TC exp (empty-tenv))))

;; Type check an abstract representation of an open LC expression
;; exp: an abstract LC expression
;; tenv: an environment that associates variables with types
;; result: the type of exp in tenv
;; effect: error if the types don't work out
(define TC
  (lambda (exp tenv)
    (cond
      ((var? exp) (lookup (var-name exp)
                          tenv
                          (lambda () (error 'TC "free var: ~s" exp))))
      ((const? exp) NumT)
      ((boolV? exp) BoolT)
      ((add? exp) 
       (Type=? (TC (add-left exp) tenv) NumT (add-left exp))
       (Type=? (TC (add-right exp) tenv) NumT (add-right exp)))
      ((if0? exp)
       (Type=? (TC (if0-tst exp) tenv) BoolT (if0-tst exp))
       (Type=? (TC (if0-thn exp) tenv) (TC (if0-els exp) tenv) exp))
      ((setter? exp)
       (unless (var? (setter-lhs exp))
           (error 'TC "var  expected for var but ~s"
                  (setter-lhs exp)))
       (Type=? (TC (setter-lhs exp) tenv) (TC (setter-rhs exp) tenv) exp))
      ((app? exp)
       (let ((funT (TC (app-rator exp) tenv)))
         (unless (->? funT)
           (error 'TC "function type expected for ~s~n  ~s inferred~n"
                  (app-rator exp) funT))
         (Type=? (->-domain funT)
                 (TC (app-rand exp) tenv)
                 (app-rand exp))
         (->-range funT)))
      ((proc? exp)
       (let ((ptype (proc-type exp)))
         (make--> ptype
                  (TC (proc-body exp)
                      (extend tenv (proc-param exp) ptype)))))
      ((rec? exp)
       (let* ((var-type (rec-type exp))
              (rtenv (extend tenv (rec-var exp) var-type)))
         (Type=? (TC (rec-rhs exp) rtenv) var-type (rec-rhs exp))
         (TC (rec-body exp) rtenv)))
      (else (error 'TC "impossible: ~s" exp)))))

;; Compare two types, raise error if mismatched
;; recd: the type that was inferred for exp
;; expected: the expected type (according to context)
;; exp: an expression 
;; result: recd if recd and expected are structurally identical types
;; effect: cal to error with are if the types don't match
(define Type=?
  (lambda (recd expected b)
    (if (equal? recd expected) recd
        (error 'Type=? "expected: ~s; constructed: ~s~n   for ~s"
               expected recd exp))))

(module+ test
  (require rackunit)
  (check-equal? (TC (const 1) empty-tenv)
                (TC (parse '1) empty-tenv))
  (check-equal? (TC (proc    'x NumT  (add (var 'x) (const 1)))empty-tenv)
                (TC (parse '(lambda(x : int) (+ x 1)))empty-tenv))
  (check-equal? (TC (add (const 1)(const 2))empty-tenv)
                (TC (parse '(+ 1 2))empty-tenv))
  (check-equal? (TC (app (proc  'x   NumT (add (var 'x) (const 1)))
                     (const 2))empty-tenv)
                (TC (parse '((lambda(x : int )(+ x 1))2))empty-tenv))
  (check-equal? (TC  (app (app
                      (proc   'y NumT
                              (proc   'x  NumT  (add (var 'x) (var 'y))))
                      (const 2))
                     (const 1))empty-tenv)
                (TC  (parse '(((lambda(y : int)(lambda(x : int)(+ x y)))2)1))empty-tenv))
  (check-equal? (TC (rec 'x NumT (const 1) (add (var 'x) (const 1)))empty-tenv)
                (TC (parse '(let ((x : int 1)) (+ x 1)))empty-tenv))
  
  (check-equal? (TC (if0 (boolV false) (const 2) (const 3)) empty-tenv)
                (TC (parse '(if false 2 3))empty-tenv))

  (check-equal? (TC (app (proc 'x BoolT (if0 (var 'x) (const 2) (const 3)))(boolV false)) empty-tenv)
                (TC (parse '((lambda(x : bool)(if x 2 3))false))empty-tenv))
  (check-exn exn:fail?
                (lambda()(TC (parse '((lambda(x : bool)(if x 2 3))1))empty-tenv)))
  (check-exn exn:fail?
             (lambda()
               (TC (parse '((lambda(x : bool)(+  x 1))false))empty-tenv)))
  (check-equal? NumT
                (TC (parse '(let ((fact : (int -> int)(lambda(x : int )(if false 1 (fact (+ x 1)))))) (+ (fact 4) 1)))empty-tenv))

 (check-equal? NumT
               (TC (parse '(let ((x : int 4))(set! x 3))) empty-tenv))
  )
