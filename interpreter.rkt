#lang plai-typed


; -----------------------------
;  Data definition
; -----------------------------

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])



; -----------------------------
;  Parser
; -----------------------------

;  Key Typing      "(+ 23 (+ 5 6))"     
;       |
;     read
;       |
;  S-expression    (+ 23 (+ 5 6))
;       |
;     parse
;       |
;     ArithC       (plusC (numC 23)
;                         (plusC (numC 5)
;                                (numC 6)))

; S-expr -> ArithC
; translates S-expression s to representation of Data
(define (parse [s : s-expression]) : ArithC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))


(test (parse '2) (numC 2))
(test (parse '(+ 2 3)) (plusC (numC 2) (numC 3)))
(test (parse '(+ 23 (+ 5 6)))
      (plusC (numC 23)
             (plusC (numC 5)
                    (numC 6))))
