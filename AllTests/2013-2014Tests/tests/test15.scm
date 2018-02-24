;;;file 15: adding symbols dynamicly. functions on strings(?).

(define x 1)
(define a (string->symbol "x"));bucket exists
(define b (string->symbol "y"));bucket doesn't exist
(eq? a 'x)
(eq? a 'y)
(eq? b 'x)
(eq? b 'y)

(define aa (string->symbol "x"))
(define bb (string->symbol "y"))

(eq? a aa)
(eq? b bb)
(eq? a bb)
(eq? b aa)

(string-length (make-string 5 #\a))
(string-ref (symbol->string 'x) 0)
(string-length (symbol->string 'y))
(symbol->string 'z)
