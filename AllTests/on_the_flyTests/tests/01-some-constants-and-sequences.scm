6
'(1 2 3)
 '(1 2 3 (4 5))
 '(1)
 '((1))
'((1 2))
#f
'((1 2) (3 4 (5))) 
'((1 2) (3 4 (5 "itzik"))) 
"itzik"
"itzik s"
'#(7 8 (6 0) 9)
'(1 2 3 4) 
'#(7 8 (6 0) 9)
'(1 . 2)
'(1 . (2 . 3))
'(1 . (2 3 (2 . 3)))
'(1 2 3 (2 . 3))
(begin 1 2 3 4) ; seq
(begin 1 2 '(3 4))
(begin 1 2 '(3 4 (3 4)))
(begin 1 2 '(3 4 (3 4)))
(begin 1 2 '(3 4 (3 4)) '(5 6))
'(3 (1 . 2))
(begin 1 2 '(3 4 (3 4)) '(5 . 6) 7)
(begin 1 2 (begin 8 9)) ;nested seq
'(1 . 2) ; pair with same number after
2
'(1 . 2) ; pair with different number after
3
5
'("no! " . 9)
3
'(1 . "hi hi")
'(1 2 "hello neta" 4)
'#(5 6 (7 . 8) "bye bye" 7 3/4)