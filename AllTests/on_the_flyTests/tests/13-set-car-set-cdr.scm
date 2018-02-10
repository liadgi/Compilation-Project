(define c (cons 1 2))
c
(set-car! c 3)
c
(set-car! c 2)
c
(set-car! c "a")
c

(set-cdr! c 9)
c

(set-cdr! c '(1 2))
c

(cdr c)

(define c (list 1 2 3 4))
(define d (cons 4 5))

(define e (cdr (cdr c)))
(set-car! e d)
e

"(cdr (cdr c))"
(cdr (cdr c))
"(set-car! (cdr (cdr c)) d)"
(set-car! (cdr (cdr c)) d)
c
"(set-cdr! (cdr (cdr c)) d)"
(set-cdr! (cdr (cdr c)) d)
c

;(set-car! c '(1 2 3))