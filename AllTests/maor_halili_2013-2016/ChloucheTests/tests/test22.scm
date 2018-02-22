 ((lambda (a b c) ((lambda (e f) ((lambda (x y z) x) e f a)) a b)) 1 2 3)  ; tail position application - check the the env expansion works properly, should return 1
