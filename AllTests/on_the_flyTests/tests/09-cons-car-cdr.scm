(define a (cons 1 2))
a
(car a)

(cdr a)
(define b (list 1 2 3))
b
(car b)
(cdr b)
(cdr (cdr b))
(cdr (cdr (cdr b)))
b
(boolean? 3)
(define s #f)
(boolean? s)
12345
(define s #\c)
(char? s)
(define s #\3)
(char? s)

(define s 4)
(char? s)

11111111
(boolean? (cdr (cons (cons 1 2) #\k)))
(char? (cdr (cons (cons 1 2) #\k)))
(char? (cdr (cons (cons 1 2) 3)))
(boolean? (cdr (cons (cons 1 2) 3)))
(boolean? (cdr (cons (cons 1 2) #f)))
222222222
(boolean? (car (list #f (cons 1 2))))
(char? (car (list #f (cons 1 2))))
(char? (car (list #\e (cons 1 2))))