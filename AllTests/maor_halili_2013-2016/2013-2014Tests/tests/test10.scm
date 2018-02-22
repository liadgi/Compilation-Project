;;; file 10

(define fact
  (lambda (n)
    (if (zero? n)
1
(* n (fact (- n 1))))))

(fact 5)
