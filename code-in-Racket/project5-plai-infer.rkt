#lang racket
;; Revised definition of the proc constructor required by Typed LC
(define-struct var (name) #:transparent)
(define-struct proc (param  body) #:transparent)
(define-struct boolV (bool) #:transparent)
(define-struct const (num) #:transparent)
(define-struct add (left right) #:transparent)
(define-struct if0 (tst  thn els) #:transparent)
(define-struct app (rator rand) #:transparent)
(define-struct rec (var rhs body) #:transparent)
(define-struct setter (lhs rhs) #:transparent)
(define-struct begin0 (first rest) #:transparent)

(define-struct nullV () #:transparent)

(define-struct closV (arg body env)#:transparent)


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
          (if(symbol=? x 'true)
             #t
             #f))))


(define parse
  (lambda(M)
    (match M
      [(? integer?)
       (const M)]
      [(? boolean? b)
       (boolV b)]
      [(? is-Boolean? b) 
       (boolV (symbol->boolean b))]
      [(? symbol?)
       (if(eq? M 'lambda)
          (error M "lambda should not be a identifier")
          (var M))]
      [`(+ ,a ,b)
       (add (parse a)(parse b))]
      [`(set! ,x ,rhs)
       (setter (parse x) (parse rhs))]
      [`(lambda (,(? symbol? a) )  ,b)
       (proc a 
             (parse b))]
      [`(if ,tst ,thn ,els)
       (if0 (parse tst) (parse thn) (parse els))]
      [`(let ((,(? symbol? var0)  ,exp)) ,body)
       (rec var0  (parse exp) (parse body))]
      [`(begin ,first ,rest)
       (begin0 (parse first)(parse rest))]
      [`(,rator ,rand)
       (app (parse rator)(parse rand))]
      [else
       (error M "no match expression")])))

(module+ test
  (require rackunit)
  (check-equal? (const 1) (parse '1))
  (check-equal? (var 'a) (parse 'a))
  (check-equal? (proc   'x  (add (var 'x) (const 1)))
                (parse '(lambda(x )(+ x 1))))
  (check-equal? (add (const 1)(const 2))
                (parse '(+ 1 2)))
  (check-equal? (app (proc  'x  (add (var 'x) (const 1)))
                     (const 2))
                (parse '((lambda(x )(+ x 1))2)))
  (check-equal? (app (app
                      (proc   'y 
                              (proc   'x  (add (var 'x) (var 'y))))
                      (const 2))
                     (const 1))
                (parse '(((lambda(y )(lambda(x )(+ x y)))2)1)))
  (check-equal? (rec 'x  (const 1) (add (var 'x) (const 1)))
                (parse '(let ((x  1)) (+ x 1))))
  (check-equal? (if0 (const 1) (const 2) (const 3))
                (parse '(if 1 2 3)))
  (check-equal? (setter (var 'x) (const 3))
                (parse '(set! x 3)))
  )



;; term
(define-struct -> (domain range) #:transparent)
(define-struct varT (name) #:transparent)
(define-struct numT ()#:transparent)
(define NumT (numT))
(define-struct boolT ()#:transparent)
(define BoolT (boolT))
(define-struct expT (expC)#:transparent)
(define-struct voidT ()#:transparent)
(define VoidT (voidT))

;;constraints
(define-struct eqCon (lhs rhs) #:transparent)

(define (cg exp)
  (match exp
    [(const _)(list (eqCon (expT exp) NumT))]
    [(boolV _)(list (eqCon (expT exp) BoolT))]
    [(add left right)(append (cg left)(cg right)
                             (list
                              (eqCon (expT left) NumT)
                              (eqCon (expT right) NumT)
                              (eqCon (expT exp)NumT)))]
    [(if0 tst thn els)(append (cg tst)(cg thn)(cg els)
                              (list
                               (eqCon (expT tst)BoolT)
                               (eqCon (expT thn)(expT els))
                               (eqCon (expT exp)(expT els))))]
    [(proc param body)(append (cg body)
                              (list (eqCon (expT exp)
                                           (make--> (varT param)(expT body)))))]
    #;'((lambda(x )(+ x 1))2)
    [(app rator rand)(append (cg rator)(cg rand)
                             (list (eqCon (expT rator)
                                          (make--> (expT rand)(expT exp)))))]
    [(rec v rhs body)(append (cg rhs)(cg body)
                             (list (eqCon (varT v) (expT rhs) )
                                   (eqCon (expT exp) (expT body))))]
    [(setter lhs rhs)(append (cg rhs)
                                   (list (eqCon (varT (var-name lhs))(expT rhs)  )
                                         (eqCon (expT exp) VoidT)))]
    [(begin0 first rest)(append (cg first)
                                (cg rest)
                                (list (eqCon (expT exp) (expT rest))))]
    [(? var? varn)(list (eqCon (expT exp)(varT (var-name varn))))]))

(define-struct subst (var is) #:transparent)

(define (unify cs-list)
  (unify/0 cs-list  '()))

(define (to-string x) (format "~v" x))

(define (lookup-sub t subst-list)
  (match subst-list
    [(? null?)(box #f)]
    [else
     (match-define (subst var is) (first subst-list))
     (if(equal? var t)
        (box is)
        (lookup-sub t (rest subst-list)))]))
(define (lookup-result varE0 all-list)
  (letrec ((boxV (box #f))
           (search (lambda(varE subst-list)
                    (match subst-list
                     [(? null?)boxV]
                     [else
                      (match-define (subst var is) (first subst-list))
                      (display varE)
                      (if(equal? var varE)
                         (if(or(varT? is)(expT? is))
                            (search is all-list)
                            (begin
                              (set-box! boxV is)
                              boxV))
                         (search varE (rest subst-list)))]))))
    (search varE0 all-list)))
(lookup-result (varT 'x) (list (subst (varT 'x) NumT)))

  (module+ test
    (require rackunit)
    (check-equal? (lookup-sub
                   (expT (add (var 'x) (const 1)))
                   (list
                    (subst
                     (expT (proc 'x (add (var 'x) (const 1))))
                     (-> (varT 'x) (expT (add (var 'x) (const 1)))))
                    (subst (expT (add (var 'x) (const 1))) (numT))
                    (subst (varT 'x) (numT))
                    (subst (expT (const 1)) (numT))
                    (subst (expT (var 'x)) (numT))))
                  (box NumT)))

  (define (extend-replace lhs rhs subst-list)
    (letrec
        ([is-occurs?
          (lambda(r)
            (match r
              [(-> domain range)
               (or (is-occurs? domain)
                   (is-occurs? range))]
              [else (equal? lhs r)]))]
         [replace
          (lambda(t)
            (match t
              [(-> domain range)
               (-> (replace domain)
                   (replace range))]
              [else
               (if(equal? t lhs)
                  rhs  t)]))]
         [rest-replace
          (lambda(r-list)
            (match r-list
              [(? null?)empty]
              [else
               (match-define (subst var is)(first r-list))
               (define replace-is (replace is))
               (cons (subst var replace-is)
                     (rest-replace (rest r-list)))]))])
      (cond
        [(not (is-occurs? rhs))
         (cons (subst lhs rhs)
               (rest-replace subst-list))]
        [else (error rhs "cycle in substitution")])))

  (module+ test
    (require rackunit)
    (check-equal? (extend-replace
                   (expT (add (var 'x) (const 1))) NumT
                   (list
                    (subst
                     (expT (proc 'x (add (var 'x) (const 1))))
                     (-> (varT 'x) (expT (add (var 'x) (const 1)))))
                    (subst (expT (add (var 'x) (const 1))) (numT))
                    (subst (varT 'x) (numT))
                    (subst (expT (const 1)) (numT))
                    (subst (expT (var 'x)) (numT))))
                  (list
                   (subst (expT (add (var 'x) (const 1))) (numT))
                   (subst (expT (proc 'x (add (var 'x) (const 1)))) (-> (varT 'x) (numT)))
                   (subst (expT (add (var 'x) (const 1))) (numT))
                   (subst (varT 'x) (numT))
                   (subst (expT (const 1)) (numT))
                   (subst (expT (var 'x)) (numT))))
    (check-equal? (extend-replace
                   (varT 'x) NumT
                   (list
                    (subst
                     (expT (proc 'x (add (var 'x) (const 1))))
                     (-> (varT 'x) (expT (add (var 'x) (const 1)))))
                    (subst (expT (add (var 'x) (const 1))) (numT))
                    (subst (varT 'x) (numT))
                    (subst (expT (const 1)) (numT))
                    (subst (expT (var 'x)) (numT))))
                  (list
                   (subst (varT 'x) (numT))
                   (subst
                    (expT (proc 'x (add (var 'x) (const 1))))
                    (-> (numT) (expT (add (var 'x) (const 1)))))
                   (subst (expT (add (var 'x) (const 1))) (numT))
                   (subst (varT 'x) (numT))
                   (subst (expT (const 1)) (numT))
                   (subst (expT (var 'x)) (numT)))))


  (define(unify/0 cs-list subst-list)
    (match cs-list
      [(? null?) subst-list]
      [else
       (match-define (eqCon lhs rhs) (first cs-list))
       (match lhs
         [(varT name)
          (display name)
          (match (lookup-sub lhs subst-list)
            [(box #f)
             (unify/0 #;(replace-in-constraints lhs rhs (rest cs-list))
                      (rest cs-list)
                      (extend-replace lhs rhs subst-list))]
            [(box bound)
             (unify/0 (cons (eqCon bound rhs)
                            #;(replace-in-constraints lhs rhs (rest cs-list))
                            (rest cs-list)
                            )
                      subst-list)])]
         [(expT exp)
          (match (lookup-sub lhs subst-list)
            [(box #f)
             (unify/0 #;(replace-in-constraints lhs rhs (rest cs-list))
                      (rest cs-list)
                      (extend-replace lhs rhs subst-list))]
            [(box bound)
             (unify/0 (cons (eqCon bound rhs)
                            #;(replace-in-constraints lhs rhs (rest cs-list))
                            (rest cs-list))
                      subst-list)])]
         [(numT)
          (match rhs
            [(numT)(unify/0 (rest cs-list)
                            #;(replace-in-constraints lhs rhs (rest cs-list))
                            subst-list)]
            [else (error rhs "not number and something else")]
            #;[else (unify/0 (replace-in-constraints
                              lhs rhs (rest cs-list))
                             (cons (subst lhs rhs)
                                   subst-list))]
            #;[else (unify/0 (cons (eqCon  rhs lhs)(rest cs-list))
                             subst-list)]
            )]
         [(boolT)
          (match rhs
            [(boolT)(unify/0 (rest cs-list) subst-list)]
            [else (error rhs "not boolean and something else")])]
         [(voidT)
          (match rhs
            [(voidT)(unify/0 (rest cs-list) subst-list)]
            [else (error rhs "not boolean and something else")]
            #;[else (unify/0 (replace-in-constraints
                              lhs rhs (rest cs-list))
                             (cons (subst lhs rhs)
                                   subst-list))]
            [else (unify/0 (cons (eqCon  rhs lhs)(rest cs-list))
                             subst-list)]
            )]
         [(-> d1 r1)
          (match rhs
            [(-> d2 r2)
             (unify/0 (cons (eqCon d1 d2)
                            (cons (eqCon r1 r2)
                                   cs-list))
                      subst-list)]
            [else (error rhs "not arrow and something else")]
            #;[else (unify/0 (cons (eqCon  rhs lhs)(rest cs-list))
                             subst-list)]
            )])]))

  (module+ test
    (require rackunit)
    (unify/0 (list
              (eqCon (expT (var 'x)) (varT 'x))
              (eqCon (expT (const 1)) (numT))
              (eqCon (expT (var 'x)) (numT))
              (eqCon (expT (const 1)) (numT))
              (eqCon (expT (add (var 'x) (const 1))) (numT))
              (eqCon
               (expT (proc 'x (add (var 'x) (const 1))))
               (-> (varT 'x) (expT (add (var 'x) (const 1))))))
             (list (subst (varT 'x) NumT))
             ))

  (define (replace-in-constraints l  r cs)
    (cond
      ([empty? cs] empty)
      ([cons? cs]
       (cond
         [(equal? l (eqCon-lhs (first cs)))             
          (cons (eqCon r (eqCon-rhs (first cs)))
                (replace-in-constraints l r (rest cs)))]
         [(equal? l (eqCon-rhs (first cs)))
          (cons (eqCon (eqCon-lhs (first cs)) r)
                (replace-in-constraints l r (rest cs)))]  
         [(->? (eqCon-lhs (first cs))) (cons (eqCon (replace-in-tArrow l r (eqCon-lhs (first cs)))
                                                    (eqCon-rhs (first cs)))
                                             (replace-in-constraints l r (rest cs)))]
         [(->? (eqCon-rhs (first cs))) (begin ;(display (first cs)) (display "\n")
                                         (cons (eqCon (eqCon-lhs (first cs))
                                                      (replace-in-tArrow l r (eqCon-rhs (first cs))))
                                               (replace-in-constraints l r (rest cs))))]
         [else  (cons (first cs)
                      (replace-in-constraints l r (rest cs)))]
         ))))

  (define (replace-in-tArrow l  r term) 
    (begin ;(display "\n") (display "\n") (display (to-string l)) (display "\n") (display (to-string r)) (display "\n") (display (to-string term)) (display "\n") (display "\n")
      (cond
        ([equal? l (->-domain term)] (-> r (->-range term)))
        ([equal? l (->-range term)] (-> (->-domain term) r)) 
        (else term))))






  (define (type-of exp)
    (let ((parsed (parse exp)))
      (find-type parsed (unify (cg parsed)))))
  (define (find-type e theta)
    (begin (display (to-string e)) (display "\n")
           (cond
             ([empty? theta] (error 'find-type "can't find the expression in Subst"))
             ([cons? theta]
              (if (equal? (expT e) (subst-var (first theta)))
                  (term-to-type (subst-is (first theta)) theta)
                  (if (equal? (expT e) (subst-is (first theta)))
                      (term-to-type (subst-var (first theta)) theta)             
                      (find-type e (rest theta))))))))

  (define (term-to-type term theta) 
    (begin (display (to-string term))  (display "\n")
           (match term
             [(expT e) (find-type e theta)
                       ]
             [(varT s)
              (match (lookup-result term theta)
                [(box #f)(error term "can't find term")]
                [(box v) v])]
             [(numT) NumT]
             [(boolT) BoolT]
             [(voidT) VoidT]
             [(-> d r) (-> (term-to-type d theta) (term-to-type r theta))])))

  (define (psb exp)
    (unify (cg (parse exp))))

  (module+ test
    (require rackunit)
    (check-equal?  NumT  (type-of '1))
    (check-equal?  BoolT  (type-of 'false))
    (check-equal?  BoolT
                   (type-of 'true))
    (check-equal? NumT
                  (type-of '(+ 1 2)))
    
    (check-equal? (-> NumT NumT)
                  (type-of '(lambda(x )(+ x 1))))
     #;(check-equal? VoidT
                  (type-of '(let ((x  1))(set! x 3))))

    (check-equal? NumT
                  (type-of '((lambda(x)(+ x 1))2)))
    
    (check-exn exn:fail?
               (lambda()
                 (type-of '(lambda(x)(x x)))))
    
    )
  
  #;(check-equal? VoidT
                  (type-of '(let ((x  1))(set! x 3))))
  #;(check-equal? NumT
                  (type-of '(let ((x  1)) (+ x 1))))



  #;(check-equal? (-> (numT) (numT))
                  (type-of '(lambda(x)(begin (+ x 1)2))))
  #;(check-equal? BoolT
                  (type-of '(if false true false)))
  #;(check-equal? (-> (numT)(numT))
                  (type-of '(lambda(x)(+ x 1))))
  #;(check-equal? (-> (numT)(numT))
                  (type-of '(let ((y 1))(lambda(x)(+ x 1)))))
  #;(check-equal? (-> (numT)(numT))
                  (type-of '((lambda(x )(lambda(y)(+ x y)))2)))
  #;(check-equal? NumT
                  (type-of '(if false 1 2)))
  