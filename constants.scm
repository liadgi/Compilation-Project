(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

(define init-atom-constants-in-table ; add cond here by type
	(lambda (constants-list)
			(letrec ((build 
				(lambda (lst address table)
					(if (null? lst) (append (list (list (list) 1000 'T_NIL)) table)
						(build (cdr lst) (+ address 8) (append table (list (list (car lst) address 
							(let ((atom (car lst)))
								(cond ((boolean? atom) 'T_BOOL)
								  ((integer? atom) 'T_INT)
								  ((char? atom) 'T_INT)
								  ((string? atom) 'T_STRING)
								  ((number? atom) 'T_NUMBER)
								  ((rational? atom) 'T_FRACTION)
								  (else 'OTHER_TYPE))
								)
							))))
						)))) 
			(append (build (list->set (flatten constants-list)) 1008 '()) )
			)
			
		)
	)



#;(define build-constants-table
	(lambda (constants-list)
		(letrec ((build (lambda (lst table)
							(if (list? lst)
									(if (empty? lst) table
										())
								())
							))
				((build2 (lambda (element) 
							(if (list? element) 
								(if (empty? element) '()
									()) 
								(cond ((number? element) ())
									  ((symbol? element) ()) )))))

				((build-atoms (lambda (lst tbl) 
										())))
				)

			(build (list->set 
					(get-constants-list '() constants-list))
					'()))
		)
)




(define insert-to-table
	(lambda (c table address)
		(let ((elem (lookup-constant-in-table c table)))
			(if (null? elem) 
				(let ((first (lookup-constant-address-in-table (car c) table))
					   (second (lookup-constant-address-in-table (cdr c) table)))
					(append table (list `(,c ,address 'T_PAIR ,first ,second))))
				table
				)
			))
)

(define insert-all-to-table
	(lambda (lst table address)
		(if (null? lst) table
		  	(let* ((c (car lst))
		  	 		(elem (lookup-constant-in-table c table)))
				(if (null? elem) 
					(let ((first (lookup-constant-address-in-table (car c) table))
						   (second (lookup-constant-address-in-table (cdr c) table)))
						(insert-all-to-table (cdr lst) (append table (list `(,c ,address 'T_PAIR ,first ,second))) (+ address 8)))
					(insert-all-to-table (cdr lst) table address)
					)
				)
		  	))
)

(define build-pairs
	(lambda (exp)
		 (let* ((consts (get-constants-list '() exp))
		 		(table (init-atom-constants-in-table consts)))

		 (letrec ((insert-sublists-to-table 
		 	(lambda (consts table) 
		 		(cond ((null? consts) table)
		 			   ((list? (car consts))
		 			   		(insert-sublists-to-table (cdr consts)
		 			   			(insert-all-to-table (build-sublists-of-const-list (car consts) '()) table 2000)))
		 			   (else (insert-sublists-to-table (cdr consts) table))
		 		))))

		 (insert-sublists-to-table consts table)
		 


		))
	))

(define build-sublists-of-const-list
	(lambda (lst res)
		(if (null? lst) (reverse res)
			(build-sublists-of-const-list (cdr lst) (append res (list lst)))
			)
))


(define get-constants-list
	(lambda (var-list exp) 
		(letrec ((run
			(lambda (var-list exp) 
				(if (not (pair? exp)) '()
					(if (eq? (car exp) 'const) (cdr exp)
						(append var-list (run var-list (car exp)) 
										 (run var-list (cdr exp))))
				)
			)
			))
		(list->set (run var-list exp))
		)
	)
)

(define lookup-constant-in-table
	(lambda (constant table)
		(if (null? table) '()
			(if (equal? (caar table) constant) 
				(car table) 
				(lookup-constant-in-table constant (cdr table))))
		)
)

(define lookup-constant-address-in-table
	(lambda (constant table)
		(cadr (lookup-constant-in-table constant table))
))



