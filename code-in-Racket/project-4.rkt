#lang racket
(require rnrs/mutable-pairs-6)

;;syntax presentation
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
(define-struct setter (lhs rhs)#:transparent)
(define-struct setS (structE sym valueE) #:transparent)
(define-struct getS (structE sym) #:transparent)
(define-struct beginE (listE) #:transparent)
(define-struct structE (symbolE valueE) #:transparent)

;;Value
(define-struct boolV (b)#:transparent)
(define-struct const (number) #:transparent)
(define-struct closure (param body env) #:transparent)
(define-struct nullV ())
(define-struct consV (first rest) #:transparent)
(define-struct structV (symbolV valueV) #:transparent)

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
      [`(set! ,lhs ,rhs)
       (setter (parse lhs) (parse rhs))]
      [`(set! ,strucE ,sym ,valueE)
       (setS (parse strucE) (parse sym) (parse valueE))]
      [`(get ,structE ,sym)
       (getS (parse structE)(parse sym))]
      [`(record ,symE ,valueE)
       (if(not(= (length symE)(length valueE)))
          (error M "length are not match")
          (structE (map parse symE) (map parse valueE)))]
      [(list 'begin a ...)
       (beginE (map parse a))]
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
                (parse '(let ((x 1)) (+ x 1))))
  (check-equal? (setter (const 1) (const 2))
                (parse '(set! 1 2)))
  (check-equal? (beginE (list (setter (var 'x) (const 1)) (var 'x)))
                (parse '(begin (set! x 1) x)))
  (check-equal? (getS (structE (list (var 'x) (var 'y)) (list (opE '+ (const 1) (const 2)) (const 2))) (var 'y))
                (parse '(get (record (x y)((+ 1 2) 2)) y))))

;;environment
(define (extend var value env)
  (mcons (mcons var value) env))

(define (set-value! x v env)
  (if(null? env)
     (error var "free var")
     (let([kv (mcar env)])
       (if(eq?(mcar kv) (var-name x))
          (set-pair-val! kv v)
          (set-value! x v (mcdr env))))))
         
(define(lookup var env)
  (if(null? env)
     (error var "free var")
     (let([kv (mcar env)])
       (if(eq?(mcar kv) (var-name var))
          (mcdr kv)
          (lookup var (mcdr env))))))
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
      [(define first-pair (mcar env))]
      (set-pair-val! first-pair val))))
(define (set-pair-val! pair val)
  (set-cdr! pair val))


(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
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
         (if(not(boolV? test-result))
            (error M "test result are not booleanV")
            (if(boolV-b test-result)
               (eval-env consq env)
               (eval-env alt env))))]
      [(setter lhs rhs)
       (if(not(var? lhs))
          (error M "can not set to a nonvariable")
          (set-value! lhs (eval-env rhs env) env))]
      [(setS rec sym val)
       (match-define (structV symList valueList)(eval-env rec env))
       (let ((f (eval-env val env)))
         (set-box! (find sym symList valueList) f)
         f)]
      [(getS a n)
       (match-define (structV ns vs)(eval-env a env))
       (unbox (find n ns vs))]
      [(structE symList expList)
       (structV symList
                (map
                 (lambda (a) (box (eval-env a env)))
                 expList))]
      [(beginE listE)
       (last (map (lambda(exp)
                    (eval-env exp env))
                  listE))]
      [(app rator rand)
       (match-define (closure param body env0) (eval-env rator env))
       (eval-env  body (extend param (eval-env rand env) env0))]
      [else
       (error M "cannot eval this expression")])))

;; Takes a name and two parallel lists, returning an item from the
;; second list where the name matches the item from the first list.
(define (find n  ns vs)
  (cond
    [(empty? ns) (error 'interp "no such field")]
    [else (if (symbol=? (var-name n) (var-name(first ns)))
              (first vs)
              (find n (rest ns) (rest vs)))]))

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
  (check-equal? (eval-env (parse '4)empty-env)
                (eval-env (parse '(let ([x 2])(begin (set! x 3)(set! x 4)x))) empty-env))
  (check-exn exn:fail? (lambda()
                         (eval-env (parse '(((lambda(x)(lambda(y)(+ ((lambda(x)z) 2) x)))1)2)) empty-env)
                         ))
  (check-exn exn:fail? (lambda()(eval-env (parse '((lambda (x) (+ x y)) 7)) empty-env)
                         ))
  (check-exn exn:fail? (lambda()(eval-env (parse '((lambda (x) (1 x)) 7)) empty-env)
                         ))
  (check-equal? (eval-env (getS (structE (list (var 'x) (var 'y)) (list (opE '+ (const 1) (const 2)) (const 2))) (var 'y)) empty-env)
                (eval-env(parse '(get (record (x y)((+ 1 2) 2)) y)) empty-env))
  (check-equal? (const 333)
                (eval-env (parse '(let ([rec (record (x y)((+ 1 2) 2))]) (begin (set! rec x 333) (get rec x)))) '())))