






; test 51
(define (c set1 set2)
  (define s1 (car set1))
  (define s2 (car set2))
  (cond
    ((or (null? set1) (null? set2)) (append set1 set2))
    (else
       (cond
       ((= s1 s2) (cons s1 (cons (cdr set1) (cdr set2))))
       ((> s1 s2) (cons s2 (cons set1 (cdr set2))))
       ((< s1 s2) (cons s1 (cons (cdr set1) set2)))))))	   
(c '(1 2 3) '(4 6))

; test 52
(define (accumulate op init lst)
    (if (null? lst)
        init
        (op (car lst) (accumulate op init (cdr lst)))))
(accumulate * 2 '(1 2 3 4 5 6 7 8 9))

; test 53
(define f1 (lambda (x) x))
(f1 2)

; test 54
(define f2 (lambda (o a b) (o a b)))
(f2 + 5 6)

; test 55
(define f3 (lambda () (begin
                         (define foo (lambda (x) (x 5 6)))
                         (define bar (lambda (a b) (+ a b)))
                         (foo bar)
                         )
               )
    )
(f3)

; test 56
(define f4 (lambda (z) (begin
                         (define foo (lambda (x y) (x y 5 6)))
                         (define bar (lambda (op a b) (op a b)))
                         (foo bar z)
                         )
               )
    )
(f4 *)

; test 57
(define f5 (lambda () (begin
                           (define foo (lambda (x y) (x y 5 6)))
                           (define bar (lambda (op a b) (op a b)))
                           (define oop +)
                           (foo bar oop)
                           )
                 )
      )
(f5)

; test 59
(let ((square (lambda (x) (* x x)))) 33)