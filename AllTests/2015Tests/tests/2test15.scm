(define x 1)
(define a (string->symbol "x"));bucket exists
(define b (string->symbol "y"));bucket doesn't exist
(eq? a 'y)
