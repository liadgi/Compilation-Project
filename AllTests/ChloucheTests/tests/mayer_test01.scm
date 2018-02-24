((lambda (x) (x x 1000000))
 (lambda (x n)
   (if (zero? n) #t
       (x x (- n 1)))))
