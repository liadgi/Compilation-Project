" 10 #t"
(eq? 1 1)
(eq? "hello" "hello")
(eq? '1 '1) ; it works although the chuku, maybe because in compilation time its a number and not a symbol
(define d 1)
(eq? '1 d)
(eq? d '1)
(define a d)
(eq? d a)
(eq? 1 '1)
(eq? 2 '2)

" 3 #f"
(eq? 1 (- 2 1))
(eq? "hello" "hello ")
(eq? (- 2 1) (- 2 1))