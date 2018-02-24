; test 1
23

; test 2
(- 2)

; test 3
-1

; test 4
(/ 35 7)

; test 5
(* 5 7)

; test 6
(+ 53242653 35463560)

; test 7
(+ (* 564 5) (- (+ 4 5)))

; test 8
(- ( - ( - ( - ( - ( - ( - ( - (- 5)))))))))

; test 9
((lambda (a) ( + a 7)) 5)

; test 10
((lambda (a b) (a ( b (a 5 6) 8) 11)) + -)

; test 11
((lambda (a b) (if (number? a) (make-string a) b)) #t 6)

; test 12
 ((lambda (a b c) (if (= a c) (+ a c) (- b c 4))) 1 2 3)

; test 13
((lambda (a)
        (begin
          (define pi 3)
          (define e 2)
          (if (> a 64)
              (+ pi e)
              (* pi e)
              )
          )
        ) 10)

; test 14
(define sum (lambda (x) (if (= x 0) 0 (+ x (sum (- x 1))))))
 (sum 60)

; test 15
(define rec (lambda (func1 func2 init param num)
                (if (= 0 num)
                    init
                    (func1 (rec func1 func2 (func2 2 init param) param (- num 1))
                      )
                    )
                )
    )
(rec - + 5 7 20)

; test 16
(((lambda (x)
      (begin
        (define func (lambda (y)
                       (x y 5)
                       )
          )
        func)
      ) +) 65)

; test 17
((lambda (x)
      (begin
        (define func1 (lambda (a)
                        (+ a 4)
                        )
          )
        (define func2 (lambda (a)
                        (* a 4)
                        )
          )
        (func1 (func2 (func1 x))))) 11)

; test 18
((lambda (f1 f2 f3 x)
      (begin
        (define pi 3)
        (f1 (f2 (f3 pi x) x) x)
        )
      ) + - * 9)

; test 19
(define even?
          (lambda (x)
            (or (= x 0) (not (even? (- x 1))))))
(even? 129)

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
        (if (eq? (f1 x) (f2 x))
            'eq!
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
            ((> x y) "x is greater than y")
            ((and (> (+ x y) 10) (> (* x y) 40)) "string")
            )
      ) 111 11)

; test 34
((lambda (a) (if (string? a) (string->symbol a))) "a23")

; test 35
 (define (=number? exp num)
  (and (number? exp) (= exp num)))
(=number? 5 1) 

; test 36
(define (sq j)
    (cond
      ((= j 1) 1)
      (else
    (* (/ j 2) (/ j 2)))))
	
(sq 12)

; test 37
(define (a x set)
  (cond
    ((null? set) (list x))
    ((= x (car set)) set)
    ((< x (car set)) (cons x set))
    (else (cons (car set)(a x (cdr set))))))
	
(a 3 (cons 5 4))	

; test 38
(define (expmod a b m)
  (let ((even? (lambda (x)
                     (or (= x 0) (not (even? (- x 1)))))))
(cond 
   ((= b 0) 1)
   ((even? b) (remainder (expmod (remainder (* a a) m) (/ b 2) m) m))
   (else (remainder (* a (expmod a (- b 1) m)) m)))))
   
(expmod 5 13 1)

; test 39
(define (a str)
    (define (b x sum)
      (cond
        ((= (string-length str) x) sum)
        (else (b (+ x 1) (+ (char->integer (string-ref str x)) (* 5 sum))))))
    (b 0 0))
(a "hello")

; test 40
(define (b set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    (else
     (let ((s1 (car set1))
           (s2 (car set2)))
       (cond
       ((= s1 s2) (cons s1 (b (cdr set1) (cdr set2))))
       ((> s1 s2) (cons s2 (b set1 (cdr set2))))
       ((< s1 s2) (cons s1 (b (cdr set1) set2))))))))
(b `(1 2 3) `(4 5 6))

; test 41
(let ((z 2))
  (define x (lambda (x) (lambda (y z) (y x))))
  (((x (lambda () z)) (lambda (z) z) 3))
)

; test 42
((lambda (z)
     (define x (lambda (xv) (lambda (y z) (y xv))))

     (((x (lambda () z)) (lambda (zv) zv) 3))
     ) 14)

; test 43
(define a 'hello)
a

; test 44
(define b (string-length "world"))
b

; test 45
(define loop (lambda (num func param)
                 (if (zero? num)
                     param
                     (loop (- num 1) func (func param))
                     )
                 )
    )
(loop 7 (lambda (x) (+ x x)) 43)		

; test 46
(define loop2 (lambda (num func param)
                  (if (zero? num)
                      param
                      (func (loop2 (- num 1) func param)
                        )
                      )
                  )
    )
(loop2 7 (lambda (x) (+ x x)) 3)	

; test 47
(define loop3 (lambda (num func param)
                  (begin
                    (define i 0)
                    (define subloop (lambda ()
                                      (if (= i num)
                                          param
                                          (begin
                                            (set! i (+ i 1))
                                            (func param)
                                            (subloop)
                                            )
                                          )
                                      )
                      )
                    )
                  (subloop)
                  )
    )
(loop3 7 (lambda (x) (+ 8 x)) 123)

; test 48
(define loop4 (lambda (num func param)
                  (begin
                    (define i 0)
                    (define subloop (lambda ()
                                      (if (= i num)
                                          param
                                          (begin
                                            (set! i (+ i 1))
                                            (set! param (func param))
                                            (subloop)
                                            )
                                          )
                                      )
                      )
                    )
                  (subloop)
                  )
    )
(loop4 7 (lambda (x) (+ 4 x)) 1213)	

; test 49
(define loop5 (lambda (num func param)
                  (begin
                    (define i 0)
                    (define subloop (lambda ()
                                      (cond ((= i num) param)
                                        (else
                                          (begin
                                            (set! i (+ i 1))
                                            (set! param (func param))
                                            (subloop)
                                            )
                                          )
                                          )
                                      )
                      )
                    )
                  (subloop)
                  )
    )
(loop5 10 (lambda (x) (* 3 x)) 3)