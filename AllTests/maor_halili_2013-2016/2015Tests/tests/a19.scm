(define not (lambda (x) (if x #f #t)))

(define add1 (lambda (n) (+ n 1)))

(define even?
  (lambda (n)
    (zero? (remainder n 2))))

(define odd?
  (lambda (n)
    (not (zero? (remainder n 2)))))

(let ((x (* 5000 2)))
       (and (even? x) (odd? (add1 x))))