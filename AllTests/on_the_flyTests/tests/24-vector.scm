(vector 1 2 3 4 5 6)
(make-vector 5 6)
(vector-length (vector 1 2 3))
(vector-length (vector 1 2 3 4 5))
(vector-length (make-vector 0))
(vector-length (make-vector 5))
(vector-length (make-vector 5 6))
(vector? (vector 1 2 3))
(vector? (make-vector 5 6))
(vector-ref (vector 1 2 3) 1)
(vector-ref (vector 1 2 3 4 5) 4)
(vector-ref (vector 1 2 (cons 2 3) 4 #t) 4)
(vector-ref (vector 1 2 (cons 2 3) 4 #t) 2)
(define x (vector 1 2 3))
x
(vector-set! x 0 9)
x
(vector-set! x 1 9)
x
(vector-set! x 2 9)
x
(eq? (vector-ref (vector 1 2 3) 1) 2)