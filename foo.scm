
(define x 1)
;(list x (set! x 2) x)
(+ x x)




;(define x '(1 2 3))
;(eq? x '(1 2 3))

;(eq? 'mysymbol 'MySymbol)

"neta: \"hi liad\""
"sha\"lom"

;(eq? (> 2 1) (< 1 2))

;(vector)

;(list 3)
;(set! list (lambda (a) a))
;(list 3)

;(eq? (list) (list))

;(/ 1 4)
;(/ 1/2 2)
;(eq? (/ 1 4) (/ 1/2 2))

;(eq? (not #t) #f)

;(define a '("string" #() ()))
;(define b '("string" #() ()))
;(define c a)
;(eq? a b)
;(eq? c a)

;(begin 
;  (define z 5)
;  (define func (lambda (x) (+ x z)))
;  (define z 7)
;  (func 2))

;(eq? "str" "str")

;#\a#\b

;(eq? '(1) (cons 1 '()))

;(define (b set1 set2)
;  (cond
;    ((null? set1) set2)
;    ((null? set2) set1)
;    (else
;     (let ((s1 (car set1))
;           (s2 (car set2)))
;       (cond
;       ((= s1 s2) (cons s1 (b (cdr set1) (cdr set2))))
;       ((> s1 s2) (cons s2 (b set1 (cdr set2))))
;       ((< s1 s2) (cons s1 (b (cdr set1) set2))))))))
;(b `(1 2 3) `(4 5 6))  

;'(1 . () )

;(quote (1 a (* 4)))

(define pnina #\x01)
pnina

;(eq? (symbol->string 'helo) "helo")

'ackerman

;(make-vector 40000)


;(define (func . numbers)
; (if (null? numbers)
; 0
; (+ (car numbers) (apply func (cdr numbers)))))
;(func 9 8 7 6 5 4)

;(eq? "abc" (symbol->string 'abc))

;(eq? (string->symbol "aa") (string->symbol (make-string 2 #\a)))


 ;''q

 ;(char->integer #\x80)

;(define a "abc")
;(string-set! (symbol->string 'abc) 1 #\f)
;a

;(define y (make-string 4))
;y
;(string-set! y 0 #\b)
;y

;(eq? (string->symbol "i") (string->symbol (make-string 1 #\i)))

;(define x 'a)
;(eq? x 'a) 

;(char? "\n")

;((lambda (a b . x) x) 1 2)
;((lambda (a b . x) x) 1 2 3)
;((lambda (a b . x) x) 1 2 3 4)
;((lambda (a . x) x) 1 2 3 4 5 6)
;((lambda (a b . x) x) 1 2 3 4 5 6)

;(make-string 3)

;(cdr(cons (apply list '(1))
;(apply list '(2))))

;#\space

;"\""



(let ((a (make-string 4 #\c )) )
(string-set! a 0 #\")
a)



(define add (lambda (n x)
  (if (zero? n)
  x
  (add (- n 1) (+ x 1)))))
;(add 100000000 0)
;(add 4850000 0)

; error:
;(define 'ron 5) 
;(quote 4)


(/ 23957274398 32482374)
(/ 23957274398 -32482374)
(/ -23957274398 32482374)
(/ -23957274398 -32482374)


