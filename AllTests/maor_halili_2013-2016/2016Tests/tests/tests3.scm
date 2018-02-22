

; test 101
((lambda (a b) (cons a b)) 5 4)

; test 102
(char->integer (string-ref "abcd" 0))

; test 103
(boolean? (procedure? (lambda () (make-string 5))))

; test 104
((lambda (a) (boolean? a)) #t)

; test 105
(define a (make-string 1))
(string-set! a 0
             (integer->char 80))
a

; test 106
(pair? (cons 4 6))

; test 107
((lambda (a b) (cons a b)) 55 6)

; test 108
(pair? (lambda (a b) (cons a b)))

; test 109
((lambda (a b) (pair? (cons a b))) 1234 5678)

; test 110
(procedure? (lambda (a b) (cons a b)))

; test 111
(zero? 5)

; test 112
(not (zero? 5))

; test 113
(define a (lambda (b) (rational? b)))
(a 56)

; test 114
(define a (lambda (b) (not (rational? b))))
(a 56)

; test 115
(denominator (/ 10 2))

; test 116
(numerator 100/50)

; test 117
(define a (lambda (x y) (if (not (zero? x)) (denominator (/ y x)) (numerator y))))
(a 0 5)

; test 119
(define x (lambda (a b) (if (> (string-length a) b) (string-ref a b) a)))
(char->integer (x "hello" 3))

; test 120
(define x (lambda (a b c) (if (> (string-length a) b) (string-set! a b c) a)))
(string->symbol (x "hello" 30 #\r))

; test 121
(string->symbol ((lambda (b) (symbol->string b)) 'a))

; test 122
(vector? (make-vector 4))

; test 123
(vector-ref (vector 1 2 3 4) 0)

; test 124
((lambda (a b c d) (vector-ref (vector a b c d) 3)) 5 6 7 8)

; test 125
(define f (lambda (v) (vector? v)))
(f (vector 1 2 3 4))

; test 126
(define f (lambda (v b) (if (and (vector? v) (> (vector-length v) b))
                              (vector-ref v b)
                              `bla)))
(f (vector 1 2 3 4) 2)

; test 127
(define f (lambda (v b x) (if (and (vector? v) (> (vector-length v) b))
                                (begin
                                  (vector-set! v b x)
                                  (vector-ref v 2))
                              `bla)))
(f (vector 1 2 3 4) 2 500)

; test 128
(define f (lambda (p x) (begin
                            (set-car! p x)
                            p)))
(f (cons 4 5) 444)							

; test 129
(define f (lambda (p x) (begin
                            (set-cdr! p x)
                            p)))
(f (cons 4 5) 444)							

; test 130
(apply (lambda (a) (* a a)) '(2))
