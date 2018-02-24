; test 141
(map (lambda (x) (if (integer? x) (char->integer (integer->char x)))) '(1 2 3 + #f))

; test 142
(map (lambda (x) (if (string? x) (string->symbol x ))) '("a" "b" 3 + "f"))

; test 143
((lambda (int) (if (integer? int) (vector-ref (make-vector int 1) 0)))4)

; test 144
(begin
    (define vec (make-vector 4))
    (vector-set! vec 1 4)
    (vector-set! vec 3 'a)
    (symbol? (vector-ref vec 3))
    )

; test 145
(string? (lambda () (begin
    (define vec (make-vector 4))
    (vector-set! vec 1 4)
    (vector-set! vec 3 'b)
    (if (symbol? (vector-ref vec 3)) (symbol->string (vector-ref vec 3)))
    )))

; test 146
((lambda (vec) (vector-length vec)) (make-vector 4))

; test 147
((lambda (ch) (if (char? ch) (char->integer ch))) #\A)

; test 148
((lambda (int) (if (boolean? (char? (integer->char int))) `ok)) 5)

; test 149
 (string->symbol((lambda (str) (if (string? str) (begin
                                     (string-set! str 1 (integer->char 98))
                                      str)
                      )
      )
  "ssss"))
 
; test 150
(string->symbol ((lambda (sym int) (if (symbol? sym) (begin
                                    (set! a (symbol->string sym))
                                    (string-set! a 1 (integer->char int))
                                    a)
                            )
    ) 'abc 99))

; test 151
((lambda (list) (begin
		(set-car! (car (cdr list)) (cons 1 2))
		 list)) (list 1 (cons 22 66) 3 4))

; test 152
((lambda (list) (begin
		(set-cdr! (cdr list) (cons 1 2))
		list)) (list 1 2 3 4))

; test 153
(let* ((x 1)
         (y 2)
         (z 3))
    (+ x y z)
    )

; test 154
((lambda (x y) (
                 let* ((a x)
                       (b y)
                       )
                 (* a a b))
    ) 44 55)

; test 155
(letrec ((loop (lambda (i a) (set! a (+ (* 6 a) i)) (if (< i 10) (loop (+ i 1) a) a)))) (loop 0 0))

; test 156
(define func (lambda (lst num) (
                                  letrec ((loop
                                             (lambda (i a)
                                               (cond ((null? i)
                                                      #f)
                                                 ((eq? (car i) a) #t)
                                                 (else
                                                   (loop (cdr i) a)))
                                               )))
                                    (loop lst num)))
                 )
(func (list 1 2 3) 5)				 

; test 157
(quasiquote (0 1 2))

; test 158
(quasiquote (0 (unquote (+ 1 2)) 4))

; test 159
(quote (1 a (* 4)))

; test 160
(define q (quote (bla (((s ) s )sd ))))
q