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

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [ifS (cnd : ArithS) (cnsqnt : ArithS) (alt : ArithS)])


;   Core Language
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [ifC (cnd : ExprC) (cnsqnt : ExprC) (alt : ExprC)])


;  Function definition
(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])


; Environment
(define-type Binding
  [bind (name : symbol) (val : number)])
 
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)


;------------------
;  Data Examples
;------------------
(define double    (fdC 'double 'x (plusC (idC 'x) (idC 'x))))
(define quadruple (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x)))))
(define const5    (fdC 'const5 '_ (numC 5)))

(define fun-defs (list double quadruple const5))


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
    [numS(n) (numC n)]
    [plusS(l r) (plusC (desugar l)
                        (desugar r))]
    [multS(l r) (multC (desugar l)
                        (desugar r))]
    [uminusS(e) (multC (numC -1) (desugar e))]  ; -e defined -1*e
    [bminusS(l r) (plusC (desugar l)            ; l-r defined l+(-1)*r
                         (multC (numC -1)
                                (desugar r)))]
    [ifS(cnd cseq alt) (ifC (desugar cnd)
                            (desugar cseq)
                            (desugar alt))]))

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



; -----------------------------
;  Aux Functions
; -----------------------------

; symbol (listof FunDefC) -> FunDefC
(define (get-fundef [name : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? name (fdC-name (first fds))) (first fds)]
                   [else (get-fundef name (rest fds))])]))


; Number symbol ExprC -> ExprC
; substitutes ident 'for' within 'in' with 'what'
(define (subst [what : number] [for : symbol] [in : ExprC]) : ExprC
  (subst2 (numC what) for in))


; ExprC symbol ExprC -> ExprC
(define (subst2 [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC f (subst2 what for a))]
    [plusC (l r) (plusC (subst2 what for l)
                        (subst2 what for r))]
    [multC (l r) (multC (subst2 what for l)
                        (subst2 what for r))]
    [ifC (cnd cnsqnt alt) (ifC (subst2 what for cnd)
                               (subst2 what for cnsqnt)
                               (subst2 what for alt))]))


; symbol Env -> Number
(define (lookup [s : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup (string-append (symbol->string s) " no binding entry"))]
    [else (if (symbol=? s (bind-name (first env)))
              (bind-val (first env))
              (lookup s (rest env)))]))

(test (lookup 'x (list (bind 'x 1))) 1)
(test (lookup 'x (list (bind 'y 4) (bind 'x 1))) 1)
(test (lookup 'x (list (bind 'y 4) (bind 'x 1) (bind 'x 10))) 1)


; -----------------------------
;  Interpreter
; -----------------------------

; ArithC -> Number
; evaluates the given arithmetic expr a.
(define (interp [a : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC a
    [numC (n) n]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]
    [ifC (cnd cseq alt) (if (zero? (interp cnd env fds))
                           (interp cseq env fds)
                           (interp alt env fds))]
    [idC (s) (lookup s env)]
    [appC (fun arg) (let ((fundef (get-fundef fun fds)))
                      (interp (fdC-body fundef)
                              (extend-env (bind (fdC-arg fundef)
                                                (interp arg env fds))
                                          mt-env)
                              fds))]))

(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              mt-env
              (list (fdC 'const5 '_ (numC 5))))
      15)
 
(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      16)
 
(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      22)

(test/exn (interp (appC 'f1 (numC 3))
              mt-env
              (list (fdC 'f1 'x (appC 'f2 (numC 4)))
                    (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))))
      "lookup: x no binding entry")

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

