((lambda (x y . z)
	((lambda a (list z))
	 x))
1 2)


((lambda z
	((lambda x 
		((lambda d 
			((lambda (x y . d) 
				((lambda (r) d) "c")) 9 99)) 19 3 4)) "dfsg" "4" "c")
	) 1 2 3)


((lambda (x y . z)
	((lambda a ((lambda (x y . z)
	((lambda a z)
	 x))
1 2))
	 x))
((lambda (x y . z)
	((lambda (a) z)
	 x))
1 2) ((lambda (x y . z)
	((lambda (a) z)
	 3))
1 2))