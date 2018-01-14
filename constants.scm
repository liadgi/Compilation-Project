(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

(define init-atom-constants-in-table 
	(lambda (constants-list address)
			(letrec ((build 
				(lambda (lst table address)
					(if (null? lst) table
						(build (cdr lst) (append table (list (list (car lst) address 
							(let ((atom (car lst)))
								(cond ((boolean? atom) 'T_BOOL)
								  ((integer? atom) 'T_INT)
								  ((char? atom) 'T_CHAR)
								  ((string? atom) 'T_STRING)
								  ((number? atom) 'T_NUMBER)
								  ((rational? atom) 'T_FRACTION)
								  (else 'OTHER_TYPE))
								)
							)))
						(+ address 8))
						)))) 
			(append (build (list->set (flatten constants-list)) (list (list (list) address 'T_NIL)) (+ address 8)) )
			)
			
		)
	)




#;(define insert-to-table
	(lambda (c table address)
		(let ((elem (lookup-constant-in-table c table)))
			(if (null? elem) 
				(let ((first (lookup-constant-get-address (car c) table))
					   (second (lookup-constant-get-address (cdr c) table)))
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
					(let ((first (lookup-constant-get-address (car c) table))
						   (second (lookup-constant-get-address (cdr c) table)))
						(insert-all-to-table (cdr lst) 
							(append table (list `(,c ,address 'T_PAIR ,first ,second)))
							(+ address 8)))
					(insert-all-to-table (cdr lst) table address)
					)
				)
		  	))
)


(define build-constants-table
	(lambda (exp)
		 (let* ((consts (get-constants-list '() exp))
		 		(table (init-atom-constants-in-table consts 0)))
		 (letrec (
		 	(insert-sublists-to-table 
			 	(lambda (consts table address) 
			 		(cond ((null? consts) table)
			 			   ((list? (car consts))
			 			   		(insert-sublists-to-table (cdr consts)
			 			   			(insert-all-to-table 
			 			   				(build-sublists-of-const-list (car consts) '())
			 			   				table 
			 			   				(+ (get-last-address table) 8)) 
			 			   			(get-last-address table)))
			 			   (else (insert-sublists-to-table (cdr consts) table (get-last-address table)))
			 		))))

		 (insert-sublists-to-table consts table (get-last-address table))
		 


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

(define lookup-constant-get-address
	(lambda (constant table)
		(cadr (lookup-constant-in-table constant table))
))

(define get-last-address
	(lambda (table)
		(if (null? (cdr table)) 
			(cadar table)
			(get-last-address (cdr table)))
			)
)


