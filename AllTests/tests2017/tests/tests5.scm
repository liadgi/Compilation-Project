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
(loop5 21 (lambda (x) (* 3 x)) 123)

; test 50
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
c
