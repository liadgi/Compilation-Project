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
