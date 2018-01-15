(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

(define init-atom-constants-in-table
	(lambda (constants-list counter)
			(letrec ((build 
				(lambda (lst table counter)
					(if (null? lst) table
						(build (cdr lst) (append table 
							(list 
								(let* ((atom (car lst))
									   (label-counter (number->string counter)))
									(cond ;((boolean? atom) (list 'sob 'T_BOOL))
									  ((integer? atom) (list atom counter 'T_INT (concat-strings "sobInt" label-counter)))
									  ((char? atom) (list atom counter 'T_CHAR (concat-strings "sobChar" label-counter)))
									  ((string? atom) (list atom counter 'T_STRING (concat-strings "sobString" label-counter)))
									  ((number? atom) (list atom counter 'T_NUMBER (concat-strings "sobNumber" label-counter)))
									  ((rational? atom) (list atom counter 'T_FRACTION (concat-strings "sobRational" label-counter)))
									  (else (list atom counter "sobOTHER_TYPE" 'OTHER_TYPE))
									)
							)))
						(+ counter 1))
						)))) 
			(append (build (list->set (flatten constants-list)) (list 
				(list (list) counter 'T_NIL "sobNil") 
				(list void (+ counter 1) 'T_VOID "sobVoid") 
				(list #t (+ counter 2) 'T_BOOL "sobTrue")
				(list #f (+ counter 3) 'T_BOOL "sobFalse")) (+ counter 4)) )
			)
			
		)
	)


(define insert-all-to-table
	(lambda (lst table counter)
		(if (null? lst) table
		  	(let* ((c (car lst))
		  	 		(elem (lookup-constant-in-table c table)))
				(if (null? elem) 
					(let ((first (lookup-constant-get-label (car c) table))
						   (second (lookup-constant-get-label (cdr c) table))
						   (label-counter (number->string counter)))
						(insert-all-to-table (cdr lst) 
							(append table (list `(,c ,counter T_PAIR ,(concat-strings "sobPair" label-counter) ,first ,second)))
							(+ counter 1)))
					(insert-all-to-table (cdr lst) table counter)
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
			 			   				(+ (get-last-address table) 1)) 
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

(define lookup-constant-get-label
	(lambda (constant table)
		(cadddr (lookup-constant-in-table constant table))
))

(define get-last-address
	(lambda (table)
		(if (null? (cdr table)) 
			(cadar table)
			(get-last-address (cdr table)))
			)
)


