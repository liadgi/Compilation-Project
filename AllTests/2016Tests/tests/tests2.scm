	

; test 50
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
c

; test 51
(define (c set1 set2)
  (define s1 (car set1))
  (define s2 (car set2))
  (cond
    ((or (null? set1) (null? set2)) (append set1 set2))
    (else
       (cond
       ((= s1 s2) (cons s1 (cons (cdr set1) (cdr set2))))
       ((> s1 s2) (cons s2 (cons set1 (cdr set2))))
       ((< s1 s2) (cons s1 (cons (cdr set1) set2)))))))	   
(c `(1 2 3) `(4 6))	   

; test 52
(define (accumulate op init lst)
    (if (null? lst)
        init
        (op (car lst) (accumulate op init (cdr lst)))))
(accumulate * 2 `(1 2 3 4 5 6 7 8 9))

; test 53
(define f1 (lambda (x) x))
(f1 2)

; test 54
(define f2 (lambda (o a b) (o a b)))
(f2 + 5 6)

; test 55
(define f3 (lambda () (begin
                         (define foo (lambda (x) (x 5 6)))
                         (define bar (lambda (a b) (+ a b)))
                         (foo bar)
                         )
               )
    )
(f3)	

; test 56
(define f4 (lambda (z) (begin
                         (define foo (lambda (x y) (x y 5 6)))
                         (define bar (lambda (op a b) (op a b)))
                         (foo bar z)
                         )
               )
    )
(f4 *)	

; test 57
(define f5 (lambda () (begin
                           (define foo (lambda (x y) (x y 5 6)))
                           (define bar (lambda (op a b) (op a b)))
                           (define oop +)
                           (foo bar oop)
                           )
                 )
      )
(f5)	  		

; test 59
(let ((square (lambda (x) (* x x)))) 33)

; test 60
(define fun1 (lambda ()
                 (begin
                   (+ 2 1)
                   (+ 3 4)
                   )
                 )
    )
(fun1)

; test 61
 (define fun2 (lambda (x)
                 (begin
                   (set! x (+ 2 1))
                   (set! x (+ x 3 4))
                   x
                   )
                 )
    )
(fun2 45)

; test 62
(define fun3 (lambda ()
                 (begin
                   (define x (+ 2 1))
                   (set! x (+ x 3 4))
                   x
                   )
                 )
    )
(fun3)

; test 63
(define fun4 (lambda ()
                 (begin
                   (define f (lambda () (+ 2 1)))
                   (define x (+ (f) 3 4))
                   x
                   )
                 )
    )
(fun4)	

; test 64
(define fun5 (lambda ()
                 (begin
                   (define f (lambda () (+ 2 1)))
                   (define g (lambda () (+ (f) 3 4)))
                   g
                   )
                 )
    )
((fun5))	

; test 65
(define fun6 (lambda ()
                 (begin
                   (define f (lambda () (+ 2 1)))
                   (define g (lambda () (+ (f) 3 4)))
                   (g)
                   )
                 )
    )
(fun6)	

; test 66
(define fun7 (lambda ()
                 (begin
                   (define f (lambda (a b) (+ 2 1)))
                   (define g (lambda () (f 3 4)))
                   (g)
                   )
                 )
    )
(fun7)	

; test 67
(define fun8 (lambda ()
                 (begin
                   (define f (lambda (a b) (+ a b)))
                   (define g (lambda (f) (f 3 4)))
                   (+ (g f) (g *) (g -) (g +))
                   )
                 )
    )
(fun8)	

; test 68
(define fun9 (lambda ()
                 (begin
                   (define f (lambda (a b) (+ a b)))
                   (define g (lambda (f) (f 3 4)))
                   (define t (lambda (a) (
                                         if (eq? a *)
                                             *
                                             a)))
                   (+ (g f) (g (t *)) (g -) (g (t -)))
                   )
                 )
    )
(fun9)	

; test 70
(define fool (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda () (+ a x y)))
                            (set! a (+ (f) (f) (f)))
                            a)
                 )
    )
(fool 2 3)	

; test 71
(define foo2 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda () (+ a x y)))
                            (set! a (f))
                            (set! a (f))
                            (set! a (f))
                            a)
                 )
    )
(foo2 50 60)	

; test 72
(define foo3 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda (b) (+ b x y)))
                            (set! a (f a))
                            (set! a (f a))
                            (set! a (f a))
                            a)
                 )
    )
(foo3 43 3)

; test 73
(define foo4 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda (b) (+ b x y)))
                            (define g (lambda () (set! x 5)))
                            a)
                 )
    )
(foo4 31 3)

; test 74
(define foo5 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda (b) (+ b x y)))
                            (define g (lambda () (set! x 5)))
                            (g)
                            (f x))
                 )
    )
(foo5 11 4)

; test 75
(define foo6 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda (b) (+ b a x y)))
                            (define g (lambda () (set! x 5)))
                            (define t (lambda () (set! a y)))
                            (g)
                            (t)
                            (f x))
                 )
    )
(foo6 101 3)

; test 76
(define foo7 (lambda (x y) (
                            begin
                            (set! y x)
                            (set! x y)
                            (+ y x))
                 )
    )
(foo7 1 3)

; test 77
(define foo8 (lambda (x y) (
                            begin
                            (define y x)
                            (+ y x))
                 )
    )
(foo8 2 3)	

; test 78
(define foo9 (lambda (x y) (
                            begin
                            (define y x)
                            (eq? y x))
                 )
    )
(foo9 12 8)	

; test 79
(define foo10 (lambda (x y) (
                            begin
                            (set! y x)
                            (eq? y x))
                 )
    )
(foo10 12 12)	

; test 80
(define bar1 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (define num b)
                  (cond ((eq? num 0) a)
                    (else
                      (bar1 (rec1 a) (- b 1)))
                    )
                  )
                )
    )
	
(bar1 4 3)

; test 81
(define bar2 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (set! b (- b 1))
                  (cond ((eq? b 0) a)
                    (else
                      (bar2 (rec1 a) b))
                    )
                  )
                )
    )
	
(bar2 4 4)

; test 82
(define bar3 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (define rec2 (lambda (b) (- b 1)))
                  (set! b (rec2 b))
                  (cond ((eq? b 0) a)
                    (else
                      (bar3 (rec1 a) b))
                    )
                  )
                )
    )
	
(bar3 6 2)	

; test 83
(define bar4 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (define rec2 (lambda (b) (- b 1)))
                  (set! b (rec2 b))
                  (cond ((eq? b 0) a)
                    (else
                      (rec1 (bar4 a b)))
                    )
                  )
                )
    )
	
(bar4 5 2)	

; test 84
(define bar5 (lambda (a b)
                  (begin
                    (define rec1 (lambda (b) (* b b)))
                    (define rec2 (lambda (b) (- b 1)))
                    (set! b (rec2 b))
                    (if (eq? b 0) a
                        (rec1 (bar5 a b)))

                    )
                  )
    )
	
(bar5 5 3)	

; test 85
(define bar6 (lambda (a b c)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b b)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r 5) rac1 rac2))
                    (rac b)
                    )
                  )
    )
	
(bar6 1 2 3)	

; test 86
(define bar7 (lambda (a b c)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b b)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r 5) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar7 (rac a) (- b 1) c))
                    )
                  )
    )
	
(bar7 5 2 6)

; test 87
(define bar8 (lambda (a b c d)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b d)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r 5) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar8 (rac a) (- b 1) c d))
                    )
                  )
    )
	
(bar8 1 5 2 6)

; test 88
(define bar9 (lambda (a b c d e)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b d)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r e) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar9 (rac a) (- b 1) c d e))
                    )
                  )
    )
(bar9 2 7 3 3 10)

; test 89
(define bar10 (lambda (a b c d e)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b d)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r e) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar10 (rac (rac1 (rac2 a))) (- b 1) c d e))
                    )
                  )
    )
(bar10 1 5 4 6 1)	

; test 90
(((lambda (x)  
    (lambda (z)
      (* x x))) 4) 5)

; test 91
((lambda () (+)))

; test 92
((((lambda () (lambda (aa) (lambda (bb) (+ aa bb))))) 55) 66)

; test 93
((((lambda () (lambda (aa) (lambda (bb) (- aa bb))))) 55) 66)

; test 94
((((lambda () (lambda (aa) (lambda (bb) (+ aa bb))))) 30) 4)

; test 95
((lambda (a b c d) (a (b (c d)))) + - * 4)

; test 96
(define tar1 (lambda (a)
                (begin
                  (define r a)
                  (if (= r 1) 1 (+ 1 (tar1 (- r 1)))))))
				  
(tar1 50)				  

; test 97
(define tar2 (lambda (a)
                (begin
                  (define r a)
                  (cond ((= r 1) 1)
                   (else (* 2 (tar2 (- r 1))))))))
				  
(tar2 5)				  

; test 98
(define bin2dec (lambda (x y)
                    (begin
                      (define rem (remainder x 10))
                      (set! y (+ (* y 2) (* y rem)))
                      (if (= x 0)
                          y
                          (bin2dec (remainder x 10) y)
                          )
                      )
                    )
    )
(bin2dec 1000 2)	

; test 99
(define rem (lambda (x)(remainder x 10)))
(rem 443)

; test 100
(define f (lambda (b) (/ 200 b)))
(f 4)