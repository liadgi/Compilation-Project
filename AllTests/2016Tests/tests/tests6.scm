

; test 161
(quasiquote (1 2 (unquote (+ 3 4))))

; test 162
(quasiquote ( a 3 4 (unquote (* 4 3 2 1))))

; test 163
(begin
    (define m '(b c))
    (define n '(d e))
    `(a ,(list m n) f)
  )

; test 164
`(,(list 3 4 5))

; test 165
`(,@(list (list 3 4 5)))

; test 166
(let* ((a 1) (b 1) (c (* a b)))
   c)

; test 167
(define (lst . x) x)
(lst 1 2 3 4 5 6)

; test 168
(define (func . numbers)
    (if (null? numbers)
        0
        (+ (car numbers) (apply func (cdr numbers)))))
(func 9 8 7 6 5 4)		

; test 169
(define (f . x) (apply + x))
(f 5 4 8 6)

; test 170
(define f (lambda (x) 
            (if
             (= x 0) `done
             (begin
               (set! x (- x 1))
               (f x)
               )
             
             )
            )
  )
	
(f 3425)

; test 171
(define komodo 'komodo)
(define armadilo 'armadilo)
((lambda (x y)
                  (if (> (string-length (symbol->string x)) (string-length (symbol->string y)))
                      `(,x is greater than ,y)
                      )
                  )
 armadilo komodo)

; test 172
;; (define y #t)
;; (define is_opher (lambda (x)
;;                    (if (= x 0)
;;                        y
;;                        (begin
;;                          (set! y #f)
;;                          (is_opher (- x 1))
;;                          )
;;                        )
;;                    )
;;   )
;; `(,(is_opher 1) ,(is_opher 0))
                           

; test 173
(let ((x '(1 3 5 7 9)))
  (pair?  (cdr (cdr (cdr (cdr (cdr  x)))))))

; test 174
(define (plusminus . l)
    (if (null? l) 0
        (if (null? (cdr l)) (car l)
        (+ (- (car l) (car (cdr l))) (apply plusminus (cdr (cdr l)))))))
(plusminus 5 4 8 6 7 2 3 0 5 4 8 9 0)

; test 175
(define (less-than  . l)
     (cond
       ((null? l) #t)
       ((null? (cdr l)) #t)
       ((< (car l) (car (cdr l))) (apply less-than  (cdr l)))
       (else #f)))
	   
(less-than 5 4 8 9 6 2 5 4 4 44)	   

; test 176
(procedure? (lambda () (make-string 5)))


; test 177
(((((lambda (a)
      (lambda (b)
        (((lambda (a) (lambda (b) ((a b) (lambda (x) (lambda (y) y)))))
	  ((lambda (n)
	     ((n (lambda (x) (lambda (x) (lambda (y) y))))
	      (lambda (x) (lambda (y) x))))
	   (((lambda (a)
	       (lambda (b)
		 ((b (lambda (n)
		       ((lambda (p) (p (lambda (a) (lambda (b) b))))
			((n (lambda (p)
			      (((lambda (a)
				  (lambda (b) (lambda (c) ((c a) b))))
				((lambda (n)
				   (lambda (s)
				     (lambda (z) (s ((n s) z)))))
				 ((lambda (p)
				    (p (lambda (a) (lambda (b) a))))
				  p)))
			       ((lambda (p)
				  (p (lambda (a) (lambda (b) a))))
				p))))
			 (((lambda (a)
			     (lambda (b) (lambda (c) ((c a) b))))
			   (lambda (x) (lambda (y) y)))
			  (lambda (x) (lambda (y) y)))))))
		  a)))
	     a)
	    b)))
	 ((lambda (n)
	    ((n (lambda (x) (lambda (x) (lambda (y) y))))
	     (lambda (x) (lambda (y) x))))
	  (((lambda (a)
	      (lambda (b)
		((b (lambda (n)
		      ((lambda (p) (p (lambda (a) (lambda (b) b))))
		       ((n (lambda (p)
			     (((lambda (a)
				 (lambda (b) (lambda (c) ((c a) b))))
			       ((lambda (n)
				  (lambda (s)
				    (lambda (z) (s ((n s) z)))))
				((lambda (p)
				   (p (lambda (a) (lambda (b) a))))
				 p)))
			      ((lambda (p)
				 (p (lambda (a) (lambda (b) a))))
			       p))))
			(((lambda (a)
			    (lambda (b) (lambda (c) ((c a) b))))
			  (lambda (x) (lambda (y) y)))
			 (lambda (x) (lambda (y) y)))))))
		 a)))
	    b)
	   a)))))
    ((lambda (n)
       ((lambda (p) (p (lambda (a) (lambda (b) b))))
	((n (lambda (p)
	      (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
		((lambda (n) (lambda (s) (lambda (z) (s ((n s) z)))))
		 ((lambda (p) (p (lambda (a) (lambda (b) a)))) p)))
	       (((lambda (a)
		   (lambda (b)
		     ((b (a (lambda (a)
			      (lambda (b)
				((a (lambda (n)
				      (lambda (s)
					(lambda (z) (s ((n s) z))))))
				 b)))))
		      (lambda (x) (lambda (y) y)))))
		 ((lambda (p) (p (lambda (a) (lambda (b) a)))) p))
		((lambda (p) (p (lambda (a) (lambda (b) b)))) p)))))
	 (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
	   (lambda (x) x))
	  (lambda (x) x)))))
     (lambda (x) (lambda (y) (x (x (x (x (x y)))))))))
   (((lambda (a)
       (lambda (b)
	 ((b (a (lambda (a)
		  (lambda (b)
		    ((a (lambda (n)
			  (lambda (s) (lambda (z) (s ((n s) z))))))
		     b)))))
	  (lambda (x) (lambda (y) y)))))
     (((lambda (a)
	 (lambda (b)
	   ((b (a (lambda (a)
		    (lambda (b)
		      ((a (lambda (n)
			    (lambda (s) (lambda (z) (s ((n s) z))))))
		       b)))))
	    (lambda (x) (lambda (y) y)))))
       ((lambda (x) (lambda (y) (x (x (x y)))))
	(lambda (x) (lambda (y) (x (x y))))))
      (lambda (x) (lambda (y) (x (x (x y)))))))
    (lambda (x) (lambda (y) (x (x (x (x (x y)))))))))
  #t)
 #f)

; test 178
((lambda (x) (x x 10000))
 (lambda (x n)
   (if (zero? n) #t
       (x x (- n 1)))))

; test 179
(define not (lambda (x) (if x #f #t)))

(and
 (boolean? #t)
 (boolean? #f)
 (not (boolean? 1234))
 (not (boolean? 'a))
 (symbol? 'b)
 (procedure? procedure?)
 (eq? (car '(a b c)) 'a)
 (= (car (cons 1 2)) 1)
 (integer? 1234)
 (char? #\a)
 (null? '())
 (string? "abc")
 (symbol? 'lambda)
 (vector? '#(1 2 3))
 (not (vector? 1234))
 (not (string? '#(a b c)))
 (not (string? 1234))
 (= 3 (vector-length '#(a #t ())))
 (pair? '(a . b))
 (not (pair? '()))
 (zero? 0)
 (not (zero? 234))
 (= 97 (char->integer (string-ref "abc" 0)))
 (let ((n 10000))
   (= n (string-length (make-string n))))
 (let ((n 10000))
   (= n (vector-length (make-vector n))))
 (let ((v '#(a b c)))
   (eq? 'c (vector-ref v 2)))
 (= 65 (char->integer #\A))
 (= 3 (remainder 7 4))
 (= 6 (* 1 2 3))
 (= 1 (*))
 (= 234 (* 234))
 (= 6 (+ 1 2 3))
 (zero? (+))
 (= 234 (+ 234))
 (= 1 (- 6 3 2))
 (< 1 2 3 4 5)
 (> 5 4 3 2 1)
 )

; test 180
(define positive? (lambda (n) (> n 0)))
(define even?
  (letrec ((even-1?
	    (lambda (n)
	      (or (zero? n)
		  (odd-2? (- n 1) 'odd-2))))
	   (odd-2?
	    (lambda (n _)
	      (and (positive? n)
		   (even-3? (- n 1) (+ n n) (+ n n n)))))
	   (even-3?
	    (lambda (n _1 _2)
	      (or (zero? n)
		  (odd-5? (- n 1) (+ n n) (* n n) 'odd-5 'odder-5))))
	   (odd-5?
	    (lambda (n _1 _2 _3 _4)
	      (and (positive? n)
		   (even-1? (- n 1))))))
    even-1?))

(even? 100)
