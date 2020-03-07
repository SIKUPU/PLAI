#lang plai-typed


; -----------------------------
;  Data definition
; -----------------------------

;   Surface Language

; num
; (+ expr expr)
; (- expr expr)
; (- expr)
; (- expr expr)
; (* expr expr)
; (if expr expr expr)

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
;  Interpreter
; -----------------------------

; ArithC -> Number
; evaluates the given arithmetic expr a.
(define (interp [a : ExprC]) : number
  (type-case ExprC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]
    [ifC (cnd cseq alt) (if (zero? (interp cnd))
                           (interp cseq)
                           (interp alt))]
    [idC (s) 0]
    [appC (fun arg)
          0]))


(test (interp (numC 2)) 2)
(test (interp (plusC (numC 2) (numC 3))) 5)
(test (interp (multC (numC 5) (numC 6))) 30)

(test (interp (plusC (plusC (numC 2) (numC 3))
                     (multC (numC 5) (numC 6))))
      35)

(test (interp (multC (plusC (numC 2) (numC 3))
                     (multC (numC 5) (numC 6))))
      150)
