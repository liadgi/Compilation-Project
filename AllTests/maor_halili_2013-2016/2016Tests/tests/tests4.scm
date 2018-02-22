
; test 131
(let ((str `hello))
    (set! f1 (lambda () str))
    (set! f2 (lambda () (string->symbol str)))
	str
    )

; test 132
(let ((x 2) (y 3))
  (let* ((x 7)
         (z (+ x y)))
    (* z x))) 

; test 133
(let* ((x 2) (y 3))
    (let ((x 7)
           (z (+ x y)))
      (* z x)))

; test 134
(letrec ((x 2) (y 3))
    (let ((x 7)
           (z (+ x y)))
      (* z x)))

; test 135
((lambda (ls1 ls2) (append ls1 ls2)) `(1 2 3) `(q w e))

; test 136
(define bla (lambda (x y) (append (list x 2) (list 2 y))))
(bla `(1 2 3) `(q w e))

; test 137
(apply + (list 1 3 2))

; test 138
((lambda (list) (apply (lambda (x . y) (+ x 3)) list)) (list 1 3 2))

; test 139
(map number? '(1 2 3))

; test 140
(map boolean? '(#t #t #f "bla"))

