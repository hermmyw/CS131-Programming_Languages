#lang racket
;check indices
(define (helper-Ind arr sing ind)
(cond
        [(equal? (car arr) sing) ind]
  [else (helper-Ind (cdr arr) sing (add1 ind))]
)
)

(define (var-fixate a b)
        (string->symbol (string-append (symbol->string a) "!" (symbol->string b)))
)


(define (rec-lambda a b c d e)
  (if (and (equal? a b) '())
    ;both are empty     
    (list c d e)
    (if (not (equal? (car a) (car b))) (rec-lambda (cdr a) (cdr b) (cons (var-fixate (car a) (car b)) c) (cons (car a) d) (cons (car b) e)) (rec-lambda (cdr a) (cdr b) c d e)
    )
  )
)
; helper function for lambd
    

;fE is first expression and sE is second expression
(define (substitute fE sE singl dual)
  (if (equal? '() fE) '()
    (if (list? (car fE)) (cons (substitute (car fE) sE singl dual) (substitute (cdr fE) sE singl dual))
      (if (member (car fE) singl)
      (cons (let ((ct (helper-Ind singl (car fE) 0))) (list-ref dual ct)) (substitute (cdr fE) sE singl dual))
      (cons (car fE) (substitute (cdr fE) sE singl dual))
      )
    )
  )
)

(define (dbl term)
  (car (car term))
)
(define (fix-FHelper a b c d)
  (cons (expr-compare (car a) (car b)) (fix-Fst (cdr a) (cdr b) c d))
)
(define (fix-Flist a b c d)
  (cons (fix-Fst (car a) (car b) c d) (fix-Fst (cdr a) (cdr b) c d))
)


(define (fix-SHelper a b c d)
  (cons (expr-compare (car a) (car b)) (fix-Snd (cdr b) (cdr a) c d))
)

(define (fix-Slist a b c d)
  (cons (fix-Snd (car b) (car a) c d) (fix-Snd (cdr b) (cdr a) c d))
)

(define (fix-Fst a b c d) 
(cond
  [(equal? '() a) '()]
  [(and (equal? #t (list? (car a))) (equal? #f (list? (car b)))) (expr-compare a b)]
  [(and (equal? #f (list? (car a))) (equal? #t (list? (car b)))) (expr-compare a b)]
  [(and (list? (car a)) (equal? (dbl a) 'lambda))(fix-FHelper a b c d)]
  [(and (list? (car a)) (equal? (dbl b) 'lambda))(fix-FHelper a b c d)]
  [(and (list? (car a)) (equal? (dbl a) 'λ))(fix-FHelper a b c d)]
  [(and (list? (car a)) (equal? (dbl b) 'λ))(fix-FHelper a b c d)]
  [(list? (car a)) (fix-Flist a b c d)]
  [(member (car a) d) (cons (let ((ct (helper-Ind d (car a) 0))) (list-ref c ct)) (fix-Fst (cdr a) (cdr b) c d))]
  [else (cons (car a) (fix-Fst (cdr a) (cdr b) c d))]))

(define (fix-Snd b a c d) 
(cond
  [(equal? '() b) '()]
  [(and (equal? #t (list? (car a))) (equal? #f (list? (car b)))) (expr-compare a b)]
  [(and (equal? #f (list? (car a))) (equal? #t (list? (car b)))) (expr-compare a b)]
  [(and (list? (car b)) (equal? (dbl a) 'lambda))(fix-SHelper a b c d)]
  [(and (list? (car b)) (equal? (dbl b) 'lambda))(fix-SHelper a b c d)]
  [(and (list? (car b)) (equal? (dbl a) 'λ))(fix-SHelper a b c d)]
  [(and (list? (car b)) (equal? (dbl b) 'λ))(fix-SHelper a b c d)]
  [(list? (car b)) (fix-Slist a b c d)]
  [(member (car b) d) (cons (let ((ct (helper-Ind d (car b) 0))) (list-ref c ct)) (fix-Snd (cdr b) (cdr a) c d))]
  [else (cons (car b) (fix-Snd (cdr b) (cdr a) c d))]))



(define (sItem? a) 
(if (list? (cadr a))
(not (pair? (cadr a)))
#t
)
)
;first checks if list
					; returns true if its not a pair or list


(define (lambda-compare a b)
  (cond
    [(and (equal? #f (sItem? a))(equal? #f (sItem? b)))
    (if (equal? (length (cadr a)) (length (cadr b)))
      (let ((fixate (rec-lambda (cadr a) (cadr b) '() '() '()))) 
          (cond 
          [(or (equal? (car a) 'λ) (equal? (car b) 'λ))
          (cons 'λ
          (expr-compare
          (fix-Fst (cdr a) (cdr b) (car fixate) (cadr fixate))
          (fix-Snd (cdr b) (cdr a) (car fixate) (car (cddr fixate))))
          )]
          [else 
          (cons 'lambda
          (expr-compare
          (fix-Fst (cdr a) (cdr b) (car fixate) (cadr fixate))
          (fix-Snd (cdr b) (cdr a) (car fixate) (car (cddr fixate))))
          )
          ]
        )
      )
      (list-compare a b)
    )]
    [(and (equal? #t (sItem? a))(equal? #f (sItem? b))) (list-compare a b)]
    [(and (equal? #f (sItem? a))(equal? #t (sItem? b))) (list-compare a b)]
    [else     
      (let ((fixate (rec-lambda (list (cadr a)) (list (cadr b)) '() '() '()))) 
          (cond 
          [(or (equal? (car a) 'λ) (equal? (car b) 'λ))
          (cons 'λ
          (expr-compare
          (fix-Fst (cdr a) (cdr b) (car fixate) (cadr fixate))
          (fix-Snd (cdr b) (cdr a) (car fixate) (car (cddr fixate))))
          )]
          [else 
          (cons 'lambda
          (expr-compare
          (fix-Fst (cdr a) (cdr b) (car fixate) (cadr fixate))
          (fix-Snd (cdr b) (cdr a) (car fixate) (car (cddr fixate))))
          )
          ]
        )
      )
    ]
  )
)

;;;;

(define (loop-compare a b)
(cond
  [ (and (not(equal? '() a)) (not (equal? '() b)))
  (cons (expr-compare (car a) (car b)) (loop-compare (cdr a) (cdr b)))]
  [ (and (equal? '() a) (not (equal? '() b))) '()]
  [ (and (not(equal? '() a))  (equal? '() b)) '()]
  [else '()]
)
)
;recursively loop  

(define (list-compare a b)
  (cond
   [(equal? a b) a] ;checks if they are equal terms             
   [(and (boolean? a)(boolean? b))
    (if a (if b #t '%) (if b '(not %) #f))]
   [else (list 'if '% a b)]))
;compare base case      

(define (checker a b)
  (cond
  [(equal? (car a) 'lambda) (lambda-compare a b)]
  [(equal? (car a) 'quote) (list-compare a b)]
  [(equal? (car a) 'λ) (lambda-compare a b)]
  [else (loop-compare a b)]
  )
)

(define (checkFirst a b)
  (cond
  [(equal? 'lambda (car a)) (lambda-compare a b)]
  [(equal? 'lambda (car b)) (lambda-compare a b)]
  [(equal? 'λ (car a)) (lambda-compare a b)]
  [(equal? 'λ (car b)) (lambda-compare a b)]
  [(equal? 'quote (car a)) (list-compare a b)]
  [(equal? 'quote (car b)) (list-compare a b)]
  [(equal? 'if (car a)) (list-compare a b)]
  [(equal? 'if (car b)) (list-compare a b)]
  [else (loop-compare a b)]
  )
)
;if statement checker   

(define (expr-compare a b)
  (if (and (list? a) (list? b))

    (cond
    [(equal? (length a) (length b))
      (if (equal? (car a) (car b))
        (checker a b)
        (checkFirst a b)
      )]
    [else (list-compare a b)] 
    )
    (list-compare a b)
  )
)



(define (test-expr-compare x y)
  (cond
    [(and (equal? (eval (list 'let '((% #t)) (expr-compare x y) )) (eval x))
    (equal? (eval (list 'let '((% #f)) (expr-compare x y) )) (eval y))
    ) #t]
    [else #f]
  )
)


#|
(test-expr-compare '(let ((k 3)) k) '(let ((k 5)) 5))
|#

					;TEST EXPRESSION X AND Y
(define test-expr-x
        (list
        #t
        #f
        #t
        #f
            10
            10
      ;bools and nums   
		'(1 5 10)
		'(cons a b)
		'(cons a b)
		'(cons a b)
		'(list)
		'(if a b c)
      ;lists            
      '(+ 12 (- 20 12))
      '(quote (3 4))
      ; lambda special for
             
      '((lambda (a) (f a)) 1)
      '((lambda (a) (f a)) 1)
      '(λ (x y) (+ x y))
      '(let ((a b)) a)
      '(if a b c)
      ''((λ (a) a) c)
      '(+ #f ((λ (a b) (f a b)) 1 2))
      '(quoth (a b))
      '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
            a (lambda (a) a))))
          (lambda (b a) (b a)))
      ;pulled from test cases               
     )

)


(define test-expr-y
        (list
        #t
        #f
        #f
        #t
            10
            20
      ;bools and nums               
		'(1 5 10)
		'(cons a b)
		'(cons a c)
		'(list a b)
		'(list d)
		'(x a b c)
      ;lists    
      '(+ 12 (- 20 12))
      '(quote (3 4))
      ; lambda special form
      '((lambda (a) (f a)) 1)
      '((λ (a) (g a)) 2)
      '(λ (x y) (+ x y))
      '(let ((a b)) a)
      '(if a b c)
      ''((lambda (b) b) d)
      '(+ #t ((lambda (a c) (f a c)) 1 2))
      '(quoth (a c))
      '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
            a (λ (b) a))))
          (lambda (a b) (a b)))
     )
)