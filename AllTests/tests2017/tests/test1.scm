; test 1
23

; test 2
(- 2)

; test 3
-1

; test 4
(/ 35 7)

; test 5
(* 5 7)

; test 6
(+ 53242653 35463560)

; test 7
(+ (* 564 5) (- (+ 4 5)))

; test 8
(- ( - ( - ( - ( - ( - ( - ( - (- 5)))))))))

; test 9
((lambda (a) ( + a 7)) 5)

; test 10
((lambda (a b) (a ( b (a 5 6) 8) 11)) + -)

; test 11
((lambda (a b) (if (number? a) (make-string a) b)) #t 6)

; test 12
 ((lambda (a b c) (if (= a c) (+ a c) (- b c 4))) 1 2 3)