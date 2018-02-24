(define order
  (lambda (<)
    (letrec ((loop
	      (lambda (a s)
		(or (null? s)
		    (and (< a (car s))
			 (loop (car s) (cdr s)))))))
      (lambda (a . s)
	(loop a s)))))

(define not (lambda (x) (if x #f #t)))

(define foldr
  (lambda (binop final s)
    (letrec ((loop
	      (lambda (s)
		(if (null? s) final
		    (binop (car s) (loop (cdr s)))))))
      (loop s))))

(define compose
  (let ((binary-compose
	 (lambda (f g)
	   (lambda (x)
	     (f (g x))))))
    (lambda s
      (foldr binary-compose (lambda (x) x) s))))

(define caar (compose car car))
(define cddr (compose cdr cdr))
(define caddr (compose car cddr))

(define ^char-op
  (lambda (int-op)
    (lambda (ch1 ch2)
      (int-op (char->integer ch1) (char->integer ch2)))))

(define bin<=? (lambda (a b) (not (> a b))))
(define bin>=? (lambda (a b) (not (< a b))))

(define <= (order bin<=?))
(define >= (order bin>=?))

(define char=? (order (^char-op =)))
(define char<=? (order (^char-op bin<=?)))

(define char-lowercase?
  (lambda (ch)
    (and (char<=? #\a ch)
	 (char<=? ch #\z))))

(char-lowercase? #\x)