; test 20
(define odd? (lambda (x)
                 (begin
                   (define even?
            (lambda (x)
              (or (= x 0) (odd? (- x 1)))))
                   (if (even? x) #f #t)
                   )
                 )
    )
(odd? 129)

; test 21
((lambda (f1 f2 input1 input2 input3 ifval)
      (if (ifval input2 input3)
      (f1 (f2 input1 5) 40)
      (begin
        (set! f2 f1)
        (f1 (f2 input1 5) 40)
        )
      )
 ) * + 5 7 -8 >)

; test 22
((lambda (f1 f2 input1 input2 input3)
    (begin
      (define f (lambda () (f1 (f2 input1 input2) input3)))
      (f)
      )
    ) - - 1 2 3)

; test 23
((lambda (f1 f2 input1 input2 input3 ifval)
          (begin
            (define f (lambda () (f1 (f2 input1 5) 40)))
           (if (ifval input2 input3)
               (f)
               (begin
                 (set! f2 f1)
                 (f)
                 )
               )
           )
        ) * * 1 2 3 =)

; test 24
(((lambda (x y) (lambda () (+ x y))) 56 65))

; test 25
(((lambda (x y) (lambda () (+ x y))) ((lambda (a) (* a a)) 500) 2))

; test 26
(((lambda (x y) (lambda () (x 89 y))) (lambda (a b) (* a b)) 2))

; test 27
((lambda (x)
      (begin
        (define f1 (lambda (a) (+ a a)))
        (define f2 (lambda (a) (* a a)))
        (if (= (f1 x) (f2 x))
            'eq!'
            'no!
            )
        )
      ) 2)

; test 28
((lambda (f1 f2)
      (if (eq? f1 f2)
          'eq!
          'no!
          )
      ) + -)

; test 29
(define factorial
    (lambda(n)
      (if (= n 0)
        1
        (* n (factorial (- n 1))))))
(factorial 6)

; test 30
(define fibonacci
        (lambda (n)
          (if (< n 2)
              1
              (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))
(fibonacci 11)

; test 31
(define (equal? x y)
    (if (not (pair? x))
        (eq? x y)
        (and (equal? (car x) (car y))
             (equal? (cdr x) (cdr y)))))
(equal? (cons 1 2) (cons 1 3))

; test 32
(define (variable? x) (symbol? x))
(variable? #t)

; test 33
((lambda (x y)
      (cond ((= x y) #t)
            ((> x y) 'x>y)
            ((and (> (+ x y) 10) (> (* x y) 40)) 'string)
            )
      ) 111 11)

; test 34
((lambda (a) (if (string? a) (string->symbol a))) "a23")

; test 35
 (define (=number? exp num)
  (and (number? exp) (= exp num)))
(=number? 5 1)

; test 37
(define (a x set)
  (cond
    ((null? set) (list x))
    ((= x (car set)) set)
    ((< x (car set)) (cons x set))
    (else (cons (car set)(a x (cdr set))))))
	
(a 3 (cons 5 4))