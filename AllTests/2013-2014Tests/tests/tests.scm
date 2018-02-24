(define add1 (lambda (n) (+ n 1))) 

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

(define char-uppercase?
  (lambda (ch)
    (and (char<=? #\A ch)
	 (char<=? ch #\Z))))

(define char-lowercase?
  (lambda (ch)
    (and (char<=? #\a ch)
	 (char<=? ch #\z))))

(define char-upcase
  (let ((char-aA (- (char->integer #\a) (char->integer #\A))))
    (lambda (ch)
      (if (char-lowercase? ch)
	  (integer->char
	   (- (char->integer ch) char-aA))
	  ch))))

(define char-downcase
  (let ((char-aA (- (char->integer #\a) (char->integer #\A))))
    (lambda (ch)
      (if (char-uppercase? ch)
	  (integer->char
	   (+ (char->integer ch) char-aA))
	  ch))))

(define string-upcase
  (lambda (string)
    (list->string
     (map char-upcase (string->list string)))))

(define string-downcase
  (lambda (string)
    (list->string
     (map char-downcase (string->list string)))))

(define even?
  (lambda (n)
    (zero? (remainder n 2))))

(define odd?
  (lambda (n)
    (not (zero? (remainder n 2)))))

(define length
  (lambda (s)
    (if (null? s) 0
	(add1 (length (cdr s))))))

(define list (lambda args args))

(define list-ref
  (lambda (s i)
    (if (zero? i) (car s)
	(list-ref (cdr s) (- i 1)))))

(define map
  ((lambda (y) 
     ((lambda (map1) 
	((lambda (maplist) 
	   (lambda (f . s) 
	     (maplist f s))) 
	 (y (lambda (maplist) 
	      (lambda (f s) 
		(if (null? (car s)) '() 
		    (cons (apply f (map1 car s)) 
			  (maplist f (map1 cdr s))))))))) 
      (y (lambda (map1) 
	   (lambda (f s) 
	     (if (null? s) '() 
		 (cons (f (car s)) 
		       (map1 f (cdr s))))))))) 
   (lambda (f) 
     ((lambda (x) 
	(f (lambda (y z)
	     ((x x) y z))))
      (lambda (x) 
	(f (lambda (y z)
	     ((x x) y z))))))))

(define negative? (lambda (n) (< n 0)))

(define positive? (lambda (n) (> n 0)))

(define andmap
  (lambda (f . s)
    (letrec ((loop
	      (lambda (s)
		(or (null? (car s))
		    (and (apply f (map car s))
			 (loop (map cdr s)))))))
      (loop s))))

(define string->list
  (letrec ((loop
	    (lambda (str n s)
	      (if (= n -1) s
		  (loop str
			(- n 1)
			(cons (string-ref str n) s))))))
    (lambda (str)
      (loop str (- (string-length str) 1) '()))))

(define binary-string=?
  (lambda (str1 str2)
    (let ((n1 (string-length str1))
	  (n2 (string-length str2)))
      (and (= n1 n2)
	   (let ((s1 (string->list str1))
		 (s2 (string->list str2)))
	     (andmap char=? s1 s2))))))

(define string=? (order binary-string=?))

(define vector->list
  (letrec ((loop
	    (lambda (v n s)
	      (if (= n -1) s
		  (loop v
			(- n 1)
			(cons (vector-ref v n) s))))))
    (lambda (v)
      (loop v (- (vector-length v) 1) '()))))

(define list->string
  (lambda (s)
    (let* ((n (length s))
	   (str (make-string n)))
      (letrec ((loop
		(lambda (s i)
		  (if (= i n) str
		      (begin
			(string-set! str i (car s))
			(loop (cdr s) (+ i 1)))))))
	(loop s 0)))))

(define list->vector
  (lambda (s)
    (let* ((n (length s))
	   (v (make-vector n)))
      (letrec ((loop
		(lambda (s i)
		  (if (= i n) v
		      (begin
			(vector-set! v i (car s))
			(loop (cdr s) (+ i 1)))))))
	(loop s 0)))))

(define member?
  (lambda (a s)
    (cond ((null? s) #f)
	  ((equal? (car s) a) #t)
	  (else (member? a (cdr s))))))

(define boolean=?
  (lambda (a b)
    (if (and (boolean? a) (boolean? b))
	(if a b (not b))
	#f ; should have generated an error message!
	)))


(define equal?
  (let ((void-object (if #f #f)))
    (letrec ((equal?
	      (lambda (a b)
		(cond
		 ;; bool
		 ((and (boolean? a) (boolean? b))
		  (boolean=? a b))
		 ;; char
		 ((and (char? a) (char? b)) (char=? a b))
		 ;; nil
		 ((null? a) (null? b))
		 ;; number
		 ((and (number? a) (number? b)) (= a b))
		 ;; pair
		 ((and (pair? a) (pair? b))
		  (and (equal? (car a) (car b))
		       (equal? (cdr a) (cdr b))))
		 ;; string
		 ((and (string? a) (string? b)) (string=? a b))
		 ;; symbol
		 ((and (symbol? a) (symbol? b)) (eq? a b))
		 ;; vector
		 ((and (vector? a) (vector? b)
		       (= (vector-length a) (vector-length b)))
		  (equal? (vector->list a) (vector->list b)))
		 ;; void
		 ((eq? a void-object) (eq? b void-object))
		 (else #f)))))
      equal?)))


;;; this procedure is defined as an abstraction over assoc/assq,
;;; with the potential to define assoc over new and interesting
;;; predicates...
(define ^associate
  (lambda (equal?)
    (letrec ((run
	      (lambda (a s)
		(cond ((null? s) #f)
		      ((equal? (caar s) a) (car s))
		      (else (run a (cdr s)))))))
      run)))

(define assoc (^associate equal?))

(define assq (^associate eq?))

(define void
  (let ((void-object
	 (if #f #f)))
    (lambda () void-object)))

(define void?
  (let ((void-object (void)))
    (lambda (x) (eq? x void-object))))

(define string-append
  (lambda s
    (list->string (apply append (map string->list s)))))

(define vector-append
  (lambda s
    (list->vector (apply append (map vector->list s)))))

(define append
  (letrec ((app2
	    (lambda (s1 s2)
	      (if (null? s1) s2
		  (cons (car s1)
		   (app2 (cdr s1) s2)))))
	   (appl
	    (lambda (s1 s)
	      (if (null? s) s1
		  (app2 s1 (appl (car s) (cdr s)))))))
    (lambda s
      (if (null? s) '()
	  (appl (car s) (cdr s))))))

(define reverse
  (letrec ((loop
	    (lambda (s r)
	      (if (null? s) r
		  (loop (cdr s) (cons (car s) r))))))
    (lambda (s)
      (loop s '()))))

(define string-reverse
  (compose
   list->string
   reverse
   string->list))

(define list-set!
  (lambda (s i x)
    (if (zero? i) (set-car! s x)
	(list-set! (cdr s) (- i 1) x))))

#f
#t
'()
(or #t #t)
(if #f #f #t)
(and #t #t)
(or #f #t)
(or #f #f #f #t)
(or (or (or (or #f #f #f #f)
            (or #f #f #f #t))
        #f #f)
    #f)
(begin #f #f #f #f #t)
(begin (begin (begin #f) #f #f)
       #f #f #f #t)
((lambda (x) x) #t)
((lambda (x y) y) #f #t)
(let ((x #f) (y #t))
  (let ((x (if x #f #t))
        (y (if y #t #f)))
    (let ((x (if x #f #t))
          (y (if y #t #f)))
      (let ((x (if x #f #t))
            (y (if y #t #f)))
        (or x y)))))
(((lambda (x) x)
  (lambda (x) x)) #t)
((((lambda (x) (x x))
   (lambda (x) x))
  (lambda (x) x))
 #t)
(((((lambda (x) (x x))
    (lambda (x) (lambda (y) x)))
   #f) #t) #f)
(((((lambda (x) ((x x) (x x)))
    (lambda (x) (lambda (y) (x (x y)))))
   (lambda (x) (x (lambda (x) (lambda (y) (lambda (z) ((z y) x)))))))
  (lambda (x) ((x #f) #t)))
 (lambda (x) (lambda (y) y)))
3
'bla
'(1 2 3)
(define b '(4 5 6 78 9))
 (car b)
(define b '(4 5 6 78 9))
 (cdr b)
(define b '(4 5 6 78 9))
 (define e (cdr b))
 (car e)
(+ 85 96)
(eq? 'bla 'bla)
(= 20 20)
(apply + '(2 3 5 10))
(apply - '(10 3))
(define fact
    (lambda (n)
      (if (zero? n)
          1
          (* n (fact (- n 1))))))
 (fact 6)
(boolean? #f)
(not (boolean? 123))
(symbol? 'p)
(procedure? procedure?)
(= (car (cons 1 9)) 1)
(integer? 123)
(char? #\g)
(null? '())
(string? "abccd")
(symbol? 'lambda)
(vector? '#(1 2 3 6))
(not (vector? 1235))
(= 3 (vector-length '#(b #t ())))
(pair? '(a . o))
(zero? 0)
(let ((n 1000))
   (= n (string-length (make-string n))))   
(let ((n 1000))
   (= n (vector-length (make-vector n))))  
(let* ((v (make-vector 3))
	(vs! (lambda (i x) (vector-set! v i x))))
   (vs! 0 'a)
   (vs! 1 234)
   (vs! 2 #\P)
   (equal? v '#(a 234 #\P)))  
(let ((p (cons 'a 'b)))
   (set-car! p 'just)
   (set-cdr! p 'fine)
   (equal? p '(just . fine)))  
(let ((v '#(a b c)))
   (eq? 'c (vector-ref v 2)))   
(= 65 (char->integer #\A))
(= 3 (remainder 7 4))
(= 6 (* 1 2 3))
(= 6 (+ 1 2 3))
(= 1 (- 6 3 2))
(< 1 2 3 4 5)
(> 5 4 3 2 1)
(equal? '(2 . 3) (apply cons '(2 3)))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(eq? 'd (cadddr '(a b c d)))
(even? 4)
(odd? 5)
(equal? '(a b c d) (list 'a 'b 'c 'd))
(define (list? list) (cond ((null? list) #t)
			   ((pair? list) (list? (cdr list)))
			   (else #f)))
(list? '())
(list? '(a b c))
(equal? '(2 3 4) (map add1 '(1 2 3)))
(equal? '(5 7 9) (map + '(1 2 3) '(4 5 6)))
(member? 'do '(a b c do re mi 1 2 3))
(negative? -4)
(define max (letrec ((help (lambda (list maxv)
			 (cond ((null? list) maxv)
			       ((pair? list) (if (< maxv (car list))
						 (help (cdr list) (car list))
						 (help (cdr list) maxv)))
			       (else 'error)))))
	      (lambda list
		(cond ((pair? list) (help (cdr list) (car list)))
		      (else 'error)))))

(define min (lambda list (-(apply max (map - list)))))

(max -4 5 1 0 30 -19 50)
(min 0 1 2 3 -1 -2 -30 -40 90 -1/2 -10000/3)


(= 10 (max 4 3 2 1 6 7 8 10))
(zero? (min 4 5 6 0 7 8 9)) 

(let ((a 1))
  (let ((b 2) (c 3))
    (let ((d 4) (e 5) (f 6))
      (= 720 (* a b c d e f)))))	  
(define with (lambda (s f) (apply f s)))
 (define fact-1
  (lambda (n)
    (if (zero? n)
	(list 1 'fact-1)
	(with (fact-2 (- n 1))
	  (lambda (r . trail)
	    (cons (* n r)
	      (cons 'fact-1 trail)))))))
 (define fact-2
  (lambda (n)
    (if (zero? n)
	(list 1 'fact-2)
	(with (fact-3 (- n 1))
	  (lambda (r . trail)
	    (cons (* n r)
	      (cons 'fact-2 trail)))))))
 (define fact-3
  (lambda (n)
    (if (zero? n)
	(list 1 'fact-3)
	(with (fact-1 (- n 1))
	  (lambda (r . trail)
	    (cons (* n r)
	      (cons 'fact-3 trail)))))))
 (fact-1 6)
