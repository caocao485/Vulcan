
;;data representation

;;type tokens

;;type asts



;;方法配备模式匹配


;;method tokenize
;;token的值可以复用
#;(define (tokenize source)
  (next peek flush pushBack)) 多返回

;;借鉴cs61as project4 使用class亦可
;;或者在struct中藏闭包函数，但这样难以理解 参照cs4400 25章有关


;;method parse
(define (parse make-lexer in)
  void) 其中含有针对各种表达式的parse内部方法

;;print ast
;;也可以用更加依赖注入的方式 get-ast
;;toString方法





;;25章内容
#;(struct heap
  [ {store #:mutable}            ;; [Vectorof Value] : actual storage 
    {secondary #:mutable}         ;; [Vectorof Value] : swap-in storage 
    {next #:mutable}  ;; Nat : next place to place value into `store`
    roots             ;; [-> (Listof Location)] : find root locations
    ] #:transparent)
   
#;(define (make-plain (find-roots (lambda () '[])))
  (heap (make-vector SPACE #f) (make-vector SPACE #f) 0 find-roots))

;; print each step of the calculation that reduces `expr` to a number
; {Expr -> Number}
; print each step of the calculation that reduces ‘expr‘ to a number
#;(define (driver initial)
  (define-values (*C *E *S *K) (load initial (lambda () (find-roots *C *E *S *K))))
  (while (not (final? *C *E *S *K))
         (show-state  *C *E *S *K)
    (set!-values (*C *E *S *K) (transition *C *E *S *K)))
  (unload *C *E *S *K))
 
; {Expr  [-> [listof loc]] -> Expr Env Store Stack}
#;(define (load ae fr)
  (define plain (make-plain fr))
  (values ae empty plain mt))