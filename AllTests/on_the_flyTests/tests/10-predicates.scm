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
333333333



"integer?"
(integer? 3)

'(9 6/7 2 (66/31 (8 9 5/1)))
3/5
#t
'()
(integer? 3/5)
4
"null?"

(null? 3)
(null? 4)

(null? '())



"pair?"
(pair? '(1 . 2))
(pair? 8)
(pair? '(1 2 3))
"procedure?"
(procedure? 3)
(procedure? list)
(procedure? cons)
;(procedure? l)
;(procedure? void)
"rational?"
(rational? 3)
(rational? 3/5)

(rational? 0)
(rational? "a")
;(rational? (- 1 1))
"string?"
(string? 0)
(string? "ads")
(string? list)
"vector?"
;(vector? #(1 2 3))
(vector? '#(1 2 3))
(vector? '(1 2 3))
(vector? "hi")
"number?"
(number? 5)
(number? "hi")
(number? #t)