#lang plai-typed


; -----------------------------
;  Data definition
; -----------------------------

;   Surface Language

; expr:=
;   num
;   (+ expr expr)
;   (- expr expr)
;   (- expr)
;   (- expr expr)
;   (* expr expr)
;   (if expr expr expr)
;   (let id expr expr)

(define-type ArithS
  [numS (n : number)]
  [idS (s : symbol)]
  [plusS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [ifS (cnd : ArithS) (cnsqnt : ArithS) (alt : ArithS)]
  [letS (id : symbol) (a : ArithS) (b : ArithS)])


;   Core Language
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [ifC (cnd : ExprC) (cnsqnt : ExprC) (alt : ExprC)]
  [lamC (arg : symbol) (body : ExprC)])


; Environment
(define-type Binding
  [bind (name : symbol) (val : Value)])
 
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)


; Value
(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])


;------------------
;  Data Examples
;------------------
(define double    (lamC 'x (plusC (idC 'x) (idC 'x))))
(define quadruple (lamC 'x (appC double (appC double (idC 'x)))))
(define const5    (lamC '_ (numC 5)))

(define f2        (lamC 'y (plusC (idC 'x) (idC 'y))))
(define f1        (lamC 'x (appC f2 (numC 4))))



; -----------------------------
;  Parser
; -----------------------------

;  Key Typing      "(+ 23 (+ 5 6))"     
;       |
;     (read)
;       |
;  S-expression    (+ 23 (+ 5 6))
;       |
;   (parse ...)
;       |
;     ArithS       (plusS (numS 23)
;                         (plusS (numS 5)
;                                (numS 6)))

; S-expr -> ArithS
; translates S-expression s to representation of Data
(define (parse [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))


(test (parse '2) (numS 2))

(test (parse '(+ 2 3)) (plusS (numS 2) (numS 3)))

(test (parse '(- 2 3)) (bminusS (numS 2) (numS 3)))

(test (parse '(+ 23 (+ 5 6)))
      (plusS (numS 23)
             (plusS (numS 5)
                    (numS 6))))

(test (parse '(if 2 (+ 1 2) 5))
      (ifS (numS 2) (plusS (numS 1) (numS 2)) (numS 5)))




; -----------------------------
;  Desugar
; -----------------------------

;    ArithS
;      |
; (desugar ...)
;      |
;    ArithC

; ArithS -> ArithC
; translates the given surface language 'as' to core language
(define (desugar [as : ArithS]) : ExprC
  (type-case ArithS as
    [numS (n) (numC n)]
    [idS (s) (idC s)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [uminusS (e) (multC (numC -1) (desugar e))]  ; -e defined -1*e
    [bminusS (l r) (plusC (desugar l)            ; l-r defined l+(-1)*r
                         (multC (numC -1)
                                (desugar r)))]
    [ifS (cnd cseq alt) (ifC (desugar cnd)
                            (desugar cseq)
                            (desugar alt))]
    [letS (id arg body) (appC (lamC id (desugar body))
                              (desugar arg))]))

(test (desugar (numS 2)) (numC 2))

(test (desugar (plusS (numS 2) (numS 3)))
      (plusC (numC 2) (numC 3)))

(test (desugar (multS (numS 5) (numS 6)))
      (multC (numC 5) (numC 6)))

(test (desugar (bminusS (numS 9) (numS 8)))
      (plusC (numC 9) (multC (numC -1) (numC 8))))

(test (desugar (uminusS (numS 5)))
      (multC (numC -1) (numC 5)))

(test (desugar (uminusS (plusS (numS 5) (numS 6))))
      (multC (numC -1) (plusC (numC 5) (numC 6))))

(test (desugar (letS 'x (numS 1) (plusS (idS 'x) (numS 2))))
      (appC (lamC 'x (plusC (idC 'x) (numC 2)))
            (numC 1)))

(test (desugar (letS 'x (numS 1)
                     (letS 'y (numS 2)
                           (plusS (idS 'x) (idS 'y)))))
      (appC (lamC 'x (appC (lamC 'y (plusC (idC 'x) (idC 'y)))
                           (numC 2)))
            (numC 1)))
                  

; -----------------------------
;  Aux Functions
; -----------------------------

; symbol Env -> Value
(define (lookup [s : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup (string-append (symbol->string s) " no binding entry"))]
    [else (if (symbol=? s (bind-name (first env)))
              (bind-val (first env))
              (lookup s (rest env)))]))

(test (lookup 'x (list (bind 'x (numV 1)))) (numV 1))
(test (lookup 'x (list (bind 'y (numV 4)) (bind 'x (numV 1)))) (numV 1))
(test (lookup 'x (list (bind 'y (numV 4)) (bind 'x (numV 1)) (bind 'x (numV 10)))) (numV 1))



; -----------------------------
;  Interpreter
; -----------------------------

; ExprC -> Value
; evaluates the given arithmetic expr a.
(define (interp [a : ExprC] [env : Env]) : Value
  (local (; Value Value -> Value
          (define (num+ [l : Value] [r : Value]) : Value
            (if (and (numV? l) (numV? r))
                (numV (+ (numV-n l) (numV-n r)))
                (error 'num+ "one argument was not a number")))

          ; Value Value -> Value
          (define (num* [l : Value] [r : Value]) : Value
            (if (and (numV? l) (numV? r))
                (numV (* (numV-n l) (numV-n r)))
                (error 'num+ "one argument was not a number")))

          ; Value -> Boolean
          (define (numZero? [cnd : Value]) : boolean
            (cond
              [(numV? cnd) (zero? (numV-n cnd))]
              [else (error 'numIf "one argument was not a number")])))
    ;-----------------
    (type-case ExprC a
      [numC (n) (numV n)]
      [plusC (l r) (num+ (interp l env) (interp r env))]
      [multC (l r) (num* (interp l env) (interp r env))]
      [ifC (cnd cseq alt) (if (numZero? (interp cnd env))
                              (interp alt env)
                              (interp cseq env))]
      [idC (s) (lookup s env)]
      [lamC (a b) (closV a b env)]
      [appC (f a) (local ([define f-value (interp f env)])
                    (if (closV? f-value)
                        (interp (closV-body f-value)
                            (extend-env (bind (closV-arg f-value)
                                              (interp a env))
                                        (closV-env f-value)))
                        (error 'interp "operation position was not a function value")))])))

(test (interp (plusC (numC 10) (appC const5 (numC 10)))
              mt-env)
      (numV 15))
 
(test (interp (plusC (numC 10) (appC double (plusC (numC 1) (numC 2))))
              mt-env)
      (numV 16))
 
(test (interp (plusC (numC 10) (appC quadruple (plusC (numC 1) (numC 2))))
              mt-env)
      (numV 22))

(test/exn (interp (appC f1 (numC 3))
                  mt-env)
          "lookup: x no binding entry")

(test/exn (interp (appC (lamC 'f2 (appC (idC 'f2) (numC 4)))
                        (lamC 'y (plusC (idC 'x) (idC 'y))))
                  mt-env)
          "lookup: x no binding entry")
                        

(test/exn (interp (appC (numC 1) (numC 3))
                  mt-env)
          "interp: operation position was not a function value")

#|
(test (interp (numC 2) fun-defs) 2)
(test (interp (plusC (numC 2) (numC 3)) fun-defs) 5)
(test (interp (multC (numC 5) (numC 6)) fun-defs) 30)

(test (interp (plusC (plusC (numC 2) (numC 3))
                     (multC (numC 5) (numC 6)))
              fun-defs)
      35)

(test (interp (multC (plusC (numC 2) (numC 3))
                     (multC (numC 5) (numC 6)))
              fun-defs)
      150)

(test (interp (appC 'double (numC 2)) fun-defs)
      4)

(test (interp (appC 'double (multC (numC 2) (numC 3))) fun-defs)
      12)
|#

