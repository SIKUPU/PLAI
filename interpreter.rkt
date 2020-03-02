#lang plai-typed


; -----------------------------
;  Data definition
; -----------------------------

;   Surface Language
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)])


;   Core Language
(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])



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
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))


(test (parse '2) (numS 2))
(test (parse '(+ 2 3)) (plusS (numS 2) (numS 3)))
(test (parse '(- 2 3)) (bminusS (numS 2) (numS 3)))
(test (parse '(+ 23 (+ 5 6)))
      (plusS (numS 23)
             (plusS (numS 5)
                    (numS 6))))



; -----------------------------
;  Interpreter
; -----------------------------

; ArithC -> Number
; evaluates the given arithmetic expr a.
(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC(n) n]
    [plusC(l r) (+ (interp l) (interp r))]
    [multC(l r) (* (interp l) (interp r))]))


(test (interp (numC 2)) 2)
(test (interp (plusC (numC 2) (numC 3))) 5)
(test (interp (multC (numC 5) (numC 6))) 30)

(test (interp (plusC (plusC (numC 2) (numC 3))
                     (multC (numC 5) (numC 6))))
      35)

(test (interp (multC (plusC (numC 2) (numC 3))
                     (multC (numC 5) (numC 6))))
      150)
