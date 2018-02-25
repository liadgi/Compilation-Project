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
                            (= y x))
                 )
    )
(foo9 12 8)

; test 79
(define foo10 (lambda (x y) (
                            begin
                            (set! y x)
                            (= y x))
                 )
    )
(foo10 12 12)

; test 80
(define bar1 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (define num b)
                  (cond ((= num 0) a)
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
                  (cond ((= b 0) a)
                    (else
                      (bar2 (rec1 a) b))
                    )
                  )
                )
    )
	
(bar2 4 5)
