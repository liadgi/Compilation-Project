((lambda (x y . z)
	((lambda a (list z))
	 x))
1 2)

((lambda z
	z)
	 3
)

((lambda z
	((lambda (a) z)
	 3))
)

#;((lambda z
	((lambda (a) z (lambda (x) z))
	 3))
)

((lambda z
	((lambda (a) z)
	 3))
)

((lambda (x y . z)
	((lambda a y) x)
	)
1 2)
3
((lambda (x y . z)
	z) 1 2 3 4 5 6)




((lambda (x y . z)
	((lambda a (list z))
	 x))
1 2)

((lambda (x y . z)
	((lambda a z)
	 x))
1 2)

((lambda (x y . z)
	((lambda (a) z)
	 x))
1 2)

((lambda (x y . z)
	((lambda (a) z)
	 3))
1 2)

((lambda (x . z)
	((lambda (a) z)
	 3))
1)

((lambda z
	((lambda () ((lambda () z)))
	 ))
)

((lambda (x . z)
	((lambda (a) z)
	 3))
1)

; works after inc fix: ==================================

((lambda z
	((lambda (a) z)
	 3))
)



((lambda (x y z)
	((lambda (a b c) c) x y z)) 1 2 3)






((lambda z 
	z)
	 3
)


((lambda (x . z)
	((lambda () z)
	 ))
1)



((lambda (x y . z) ((lambda a y) x)) 1 2)
((lambda (x y . z) ((lambda (a) y) x)) 1 2)
((lambda (x y . z) y) 1 2)
((lambda (x y) ((lambda (a) y) x)) 1 2)

((lambda (x y . z)
	((lambda a y)
	 x))
1 2)


((lambda (x y . z)
	z
	)
1 2)
((lambda (x y . z)
	((lambda a z)
	 x))
1 2 3)
((lambda (x . z)
	((lambda a a)
	 x))
1 2)
((lambda (x . z)
	((lambda a z)
	 x))
1 2)
((lambda (x . z)
	((lambda a x)
	 x))
1 2)
((lambda (x . z)
	((lambda a z)
	 z))
1 2)

((lambda (x y . z)
	((lambda a (list z))
	 x))
1 2)
