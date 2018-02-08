((((lambda x
	(lambda (y z)
		(lambda (a b . c)
			x)))
		1 2 3)
	4 5)
6 7)

((((lambda x
	(lambda (y z)
		(lambda (a b . c)
			x)))
		((lambda () 1 2 3)))
	4 5)
6 7)

((((lambda x
	(lambda (y z)
		(lambda (a b . c)
			x)))
		((lambda () 1 2)))
	4 5)
6 7)

((((lambda x
	(lambda (y z)
		(lambda (a b . c)
			c)))
		((lambda () 1 2)))
	4 5)
6 7 8)

((((lambda x
	(lambda (y z)
		(lambda (a b . c)
			a)))
		((lambda () 1 2)))
	4 5)
6 7 8)

((((lambda x
	(lambda (y z)
		(lambda (a b . c)
			a)))
		((lambda () 1 2)))
	4 5)
6 7 8)

((((((lambda (x)
	(lambda (y z)
		(lambda (a b . c)
			(lambda d 
				(lambda (r s . t) t)))))
((lambda () 1 2)))
4 5)
6 7 8)
"a" "b" "k")
99 100 101 102)

((((((lambda (x)
	(lambda (y z)
		(lambda (a b . c)
			(lambda d 
				(lambda (r s . t) t))))
	(lambda (y z)
		(lambda (a b . c)
			(lambda d 
				(lambda (r s . t) t))))
	)
((lambda () 1 2)))
4 5)
6 7 8)
"a" "b" "k")
99 100 101 102)

((((((lambda x
	(lambda (y z)
		(lambda (a b . c)
			(lambda d 
				(lambda (r s . t) x))))
	(lambda (y z)
		(lambda (a b . c)
			(lambda d 
				(lambda (r s . t) t))))
	)
((lambda () 1 2)))
4 5)
6 7 8)
"a" "b" "k")
99 100 101 102)

((((((lambda x
	(lambda (y z)
		(lambda (a b . c)
			(lambda d 
				(lambda (r s . t) x))))
	(lambda (y z)
		(lambda (a b . c)
			(lambda d 
				(lambda (r s . t) x))))
	)
((lambda () 1 2)))
4 5)
6 7 8)
"a" "b" "k")
99 100 101 102)

((((((lambda x
	(lambda (y z)
		(lambda (a b . c)
			(lambda d 
				(lambda (r s . t) x))))
	(lambda (y z)
		(lambda (a b . c)
			(lambda d 
				(lambda (r s . t) x))))
	)
((lambda d 1 2)))
4 5)
6 7 8)
"a" "b" "k")
99 100 101 102)


((((((lambda x
	(lambda (y z)
		(lambda (a b . c)
			(lambda d 
				(lambda (r s . t) x))))
	(lambda (y z)
		(lambda (a b . c)
			(lambda d 
				(lambda (r s . t) x))))
	)
((lambda d 1 d)))
4 5)
6 7 8)
"a" "b" "k")
99 100 101 102)

((((((lambda x
	(lambda (y z)
		(lambda (a b . c)
			(lambda d 
				(lambda (r s . t) x))))
	(lambda (y z)
		(lambda (a b . c)
			(lambda d 
				(lambda (r s . t) (a 19)))))
	)
((lambda d 1 d)))
4 5)
(lambda k k) 7 8)
"a" "b" "k")
99 100 101 102)


((((((lambda x
	(lambda (y z)
		(lambda (a b . c)
			(lambda d 
				(lambda (r s . t) x))))
	(lambda (y z)
		(lambda (a b . c)
			(lambda d 
				(lambda (r s . t) (a 19)))))
	)
((lambda d 1 d)))
4 5)
(lambda k 6) 7 8)
"a" "b" "k")
99 100 101 102)