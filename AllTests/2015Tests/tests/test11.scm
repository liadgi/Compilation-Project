;;; file 11

(letrec ((fact-1
 (lambda (n)
   (if (zero? n)
1
(* n (fact-2 (- n 1))))))
(fact-2
 (lambda (n)
   (if (zero? n)
1
(* n (fact-3 (- n 1))))))
(fact-3
 (lambda (n)
   (if (zero? n)
1
(* n (fact-4 (- n 1))))))
(fact-4
 (lambda (n)
   (if (zero? n)
1
(* n (fact-5 (- n 1))))))
(fact-5
 (lambda (n)
   (if (zero? n)
1
(* n (fact-1 (- n 1)))))))
  (fact-1 10))
