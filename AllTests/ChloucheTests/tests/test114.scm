(define fact-tail (lambda (n acc) (if (= 1 n) acc (fact-tail (- n 1) (* n acc))))) (apply fact-tail '(5 1))                    ; 120
