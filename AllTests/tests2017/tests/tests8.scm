
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