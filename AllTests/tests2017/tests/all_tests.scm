



; test 82
(define bar3 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (define rec2 (lambda (b) (- b 1)))
                  (set! b (rec2 b))
                  (cond ((= b 0) a)
                    (else
                      (bar3 (rec1 a) b))
                    )
                  )
                )
    )
	
(bar3 6 2)

; test 83
(define bar4 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (define rec2 (lambda (b) (- b 1)))
                  (set! b (rec2 b))
                  (cond ((= b 0) a)
                    (else
                      (rec1 (bar4 a b)))
                    )
                  )
                )
    )
	
(bar4 5 2)

; test 84
(define bar5 (lambda (a b)
                  (begin
                    (define rec1 (lambda (b) (* b b)))
                    (define rec2 (lambda (b) (- b 1)))
                    (set! b (rec2 b))
                    (if (= b 0) a
                        (rec1 (bar5 a b)))

                    )
                  )
    )
	
(bar5 5 3)

; test 85
(define bar6 (lambda (a b c)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b b)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r 5) rac1 rac2))
                    (rac b)
                    )
                  )
    )
	
(bar6 1 2 3)

; test 86
(define bar7 (lambda (a b c)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b b)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r 5) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar7 (rac a) (- b 1) c))
                    )
                  )
    )
	
(bar7 5 2 6)

; test 87
(define bar8 (lambda (a b c d)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b d)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r 5) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar8 (rac a) (- b 1) c d))
                    )
                  )
    )
	
(bar8 1 5 2 6)

; test 88
(define bar9 (lambda (a b c d e)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b d)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r e) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar9 (rac a) (- b 1) c d e))
                    )
                  )
    )
(bar9 2 7 3 3 10)

; test 89
(define bar10 (lambda (a b c d e)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b d)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r e) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar10 (rac (rac1 (rac2 a))) (- b 1) c d e))
                    )
                  )
    )
(bar10 1 5 4 6 1)

; test 90
(((lambda (x)  
    (lambda (z)
      (* x x))) 4) 5)

; test 91
((lambda () (+)))

; test 92
((((lambda () (lambda (aa) (lambda (bb) (+ aa bb))))) 55) 66)

; test 93
((((lambda () (lambda (aa) (lambda (bb) (- aa bb))))) 55) 66)

; test 94
((((lambda () (lambda (aa) (lambda (bb) (+ aa bb))))) 30) 4)

; test 95
((lambda (a b c d) (a (b (c d)))) + - * 4)

; test 96
(define tar1 (lambda (a)
                (begin
                  (define r a)
                  (if (= r 1) 1 (+ 1 (tar1 (- r 1)))))))
				  
(tar1 50)

; test 97
(define tar2 (lambda (a)
                (begin
                  (define r a)
                  (cond ((= r 1) 1)
                   (else (* 2 (tar2 (- r 1))))))))
				  
(tar2 5)

; test 98
(define bin2dec (lambda (x y)
                    (begin
                      (define rem (remainder x 10))
                      (set! y (+ (* y 2) (* y rem)))
                      (if (= x 0)
                          y
                          (bin2dec (remainder x 10) y)
                          )
                      )
                    )
    )
(bin2dec 1000 2)

; test 99
(define rem (lambda (x)(remainder x 10)))
(rem 443)

; test 100
(define f (lambda (b) (/ 200 b)))
(f 4)

; test 101
((lambda (a b) (cons a b)) 5 4)

; test 103
(boolean? (procedure? (lambda () (make-string 5))))

; test 104
((lambda (a) (boolean? a)) #t)

; test 105
((lambda (a) (if (char? a) (char->integer a) (if (integer? a) (integer->char a) a))) #\x50)

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

; test 131
(let ((str 'hello))
    (set! f1 (lambda () str))
    (set! f2 (lambda () (string->symbol str)))
	str
    )

; test 132
(let ((x 2) (y 3))
  (let* ((x 7)
         (z (+ x y)))
    (* z x)))

; test 133
(let* ((x 2) (y 3))
    (let ((x 7)
           (z (+ x y)))
      (* z x)))

; test 134
(letrec ((x 2) (y 3))
    (let ((x 7)
           (z (+ x y)))
      (* z x)))

; test 135
((lambda (ls1 ls2) (append ls1 ls2)) '(1 2 3) '(q w e))

; test 136
(define bla (lambda (x y) (append (list x 2) (list 2 y))))
(bla '(1 2 3) '(q w e))

; test 137
(apply + (list 1 3 2))

; test 138
((lambda (list) (apply (lambda (x . y) (+ x 3)) list)) (list 1 3 2))

; test 139
(map number? '(1 2 3))

; test 140
(map boolean? '(#t #t #f "bla"))

; test 141
(map (lambda (x) (if (integer? x) (char->integer (integer->char x)) 0)) '(1 2 3 + #f))

; test 142
(map (lambda (x) (if (string? x) (string->symbol x) 0)) '("a1" "b2" 3 + "cf"))

; test 143
((lambda (int) (integer? int))4)

; test 144
(map number? '(1 2 '3))

; test 145
(string? '1)

; test 147
((lambda (ch) (if (char? ch) (char->integer ch))) #\x666)

; test 148
((lambda (int) (if (boolean? (char? (integer->char int))) 'ok)) 5)

; test 149
((lambda (str)
   (if (string? str)
       (begin
	 (string-set! str 1 (integer->char 66))
	 str))) "ssss")

; test 150
((lambda (sym int)
   (if (symbol? sym) (begin
		       (set! a (symbol->string sym))
		       (string-set! a 2 (integer->char int))
		       a))) 'abc 33)

; test 151
((lambda (list) (begin
		(set-car! (car (cdr list)) (cons 1 2))
		 list)) (list 1 (cons 22 66) 3 4))

; test 152
((lambda (list) (begin
		(set-cdr! (cdr list) (cons 1 2))
		list)) (list 1 2 3 4))

; test 153
(let* ((x 1)
         (y 2)
         (z 3))
    (+ x y z)
    )

; test 154
((lambda (x y) (
                 let* ((a x)
                       (b y)
                       )
                 (* a a b))
    ) 44 55)

; test 155
(letrec ((loop (lambda (i a)
		 (set! a (+ (* 10 a) i))
		 (if (< i 10)
		     (loop (+ i 1) a)
		     a))))
  (loop 0 0))

; test 156
(define func (lambda (lst num) (
                                  letrec ((loop
                                             (lambda (i a)
                                               (cond ((null? i)
                                                      #f)
                                                 ((= (car i) a) #t)
                                                 (else
                                                   (loop (cdr i) a)))
                                               )))
                                    (loop lst num)))
                 )
(func (list 1 2 3) 5)

; test 157
(quasiquote (0 1 2))

; test 158
(quasiquote (0 (unquote (+ 1 2)) 4))

; test 159
(quote (1 a (* 4)))

; test 160
(define q (quote (bla (((s ) s )sd ))))
q

; test 161
(quasiquote (1 2 (unquote (+ 3 4))))

; test 162
(quasiquote ( a 3 4 (unquote (* 4 3 2 1))))

; test 164
`(unquote (quote (3 4 5)))

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

; test 172
5

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
 (not (string? 1234))
 (pair? '(a . b))
 (not (pair? '()))
 (zero? 0)
 (not (zero? 234))
 (= 97 (char->integer (string-ref "abc" 0)))
 (let ((n 10000))
   (= n (string-length (make-string n))))
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

; test 181
(let ((a 1))
  (let ((b 2) (c 3))
    (let ((d 4) (e 5) (f 6))
      (= 720 (* a b c d e f)))))

; test 182
(define sum (lambda (n) (/ (* n (+ n 1)) 2)))
(sum 300)

; test 183
(define with (lambda (s f) (apply f s)))
(define fact
  (letrec ((fact-1
	    (lambda (n r)
	      (if (zero? n)
		  r
		  (fact-2 (- n 1)
			  (* n r)
			  'moshe
			  'yosi))))
	   (fact-2
	    (lambda (n r _1 _2)
	      (if (zero? n)
		  r
		  (fact-3 (- n 1)
			  (* n r)
			  'dana
			  'michal
			  'olga
			  'sonia))))
	   (fact-3
	    (lambda (n r _1 _2 _3 _4)
	      (if (zero? n)
		  r
		  (fact-1 (- n 1)
			  (* n r))))))
    (lambda (n)
      (fact-1 n 1))))
(fact 10)

; test 184
(define with (lambda (s f) (apply f s)))
(define list (lambda args args))
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
(fact-1 10)

; test 185
(+ 1 1/2)

; test 186
(+ 1/2 1)

; test 187
(+ 1/3 2/3)

; test 188
(+)

; test 189
(= (+ (/ 1 3) 5/3 (/ 9 27)) 7/3)

; test 190
(*)

; test 191
(or)

; test 192
(and)

; test 193
(+ 3 4 5/4 (* 1000 2/1000) 4/5 3 2 4 3/200)

; test 194
(define f (lambda (x) (if (zero? x) x (+ 1 (f (- x 1))))))
(= 50 (f 50))

; test 195
`(+ 1 ,(car '(1 2)) ,@'(2 3))

; test 196
`((unquote-splicing (quote (3 4 5))))

; test 197
`(+ ,'(+ 1 2 3) ,'(+ 2 3) (+ ,@'( 6 7)))

; test 198
`(+ ,(+ 1 2 3) ,(+ 2 3) (+ ,@'( 6 7)))

; test 199
(quasiquote (+ ,(+ 1 2 3) ,(+ 2 3) (+ (unquote-splicing '( 6 7)))))

; test 200
`(+ ,(cons 2 3) ,@'((cons 2 3)) ,'( 2 3))

