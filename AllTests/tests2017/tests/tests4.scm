
; test 38
(define (expmod a b m) 
  (cond ((= b 0) 1)
	((= (remainder b 2) 0) (remainder (expmod (remainder (* a a) m) (/ b 2) m) m))
	(else (remainder (* a (expmod a (- b 1) m)) m))))
   
(expmod 5 13 1)

; test 39
(define (a str)
    (define (b x sum)
      (cond
        ((= (string-length str) x) sum)
        (else (b (+ x 1) (+ (char->integer (string-ref str x)) (* 256 sum))))))
    (b 0 0))
(a "hello")

; test 40
(define (b set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    (else
     (let ((s1 (car set1))
           (s2 (car set2)))
       (cond
       ((= s1 s2) (cons s1 (b (cdr set1) (cdr set2))))
       ((> s1 s2) (cons s2 (b set1 (cdr set2))))
       ((< s1 s2) (cons s1 (b (cdr set1) set2))))))))
(b '(1 2 3) '(4 5 6))

; test 41
(let ((z 2))
  (define x (lambda (x) (lambda (y z) (y x))))
  (((x (lambda () z)) (lambda (z) z) 3))
)

; test 42
((lambda (z)
     (define x (lambda (xv) (lambda (y z) (y xv))))

     (((x (lambda () z)) (lambda (zv) zv) 3))
     ) 14)