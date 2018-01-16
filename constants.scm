(define parse-list-helper 
	(lambda (lst)
		(cond ((null? lst) lst)
			((number? lst) lst)
			((list? lst) `(,@(parse-list-helper (cdr lst)) ,lst))

			)
))

(define parse-pair-helper 
	(lambda (lst)
		(cond ((basic? lst) lst)
			((basic? (car lst)) (append `(,(car lst)) (all-consts `(,(cdr lst))) `(,lst)))
			((vector? (car lst)) (append (all-consts `(,(car lst))) (all-consts `(,(cdr lst))) `(,lst)))
				
			((list? (car lst)) (append (all-consts (car lst)) (parse-list-helper (car lst)) `(,(parse-pair-helper (cdr lst))) `(,lst)))

			)
))

(define basic? 
	(lambda (x)
		(or (null? x) (integer? x) (string? x) (boolean? x) (char? x) (symbol? x))))

(define all-consts
	(lambda (lst)
		(cond ((basic? lst) lst)
			((basic? (car lst)) (append `(,(car lst)) (all-consts (cdr lst))))
			((list? (car lst)) (append (all-consts (car lst)) (parse-list-helper (car lst)) (all-consts (cdr lst)) ))
			((vector? (car lst)) (append (all-consts (vector->list (car lst))) `(,(car lst)) (all-consts (cdr lst))))
			((pair? (car lst)) (parse-pair-helper (car lst)) )
			((rational? (car lst)) (append `(,(numerator (car lst))) `(,(denominator (car lst))) `(,(car lst))))
				
			)
))

(define create-records
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
									  ((pair? atom) 
									  	(let (
									  		  (first (lookup-constant-get-label (car atom) table))
										      (second (lookup-constant-get-label (cdr atom) table))
										   (label-counter (number->string counter)))
											`(,atom ,counter T_PAIR ,(concat-strings "sobPair" label-counter) ,first ,second)
											))
									  ((vector? atom) 
									  	(let ((refs (get-vector-refs (vector->list atom) '() table))
									  		(label-counter (number->string counter)))
									  		`(,atom ,counter T_VECTOR ,(concat-strings "sobVector" label-counter) ,@refs)))
									  ((rational? atom)
									  	(let ((numer (lookup-constant-get-label (numerator atom) table))
									  		(denom (lookup-constant-get-label (denominator atom) table))
									  		(label-counter (number->string counter)))
									  		`(,atom ,counter T_FRACTION ,(concat-strings "sobFraction" label-counter) ,numer ,denom)
									  	))

									  (else (list atom counter "sobOTHER_TYPE" 'OTHER_TYPE))
									)
							)))
						(+ counter 1))
						))))
			(append (build (list->set constants-list) (list 
				(list (list) counter 'T_NIL "sobNil") 
				(list void (+ counter 1) 'T_VOID "sobVoid") 
				(list #t (+ counter 2) 'T_BOOL "sobTrue")
				(list #f (+ counter 3) 'T_BOOL "sobFalse")) (+ counter 4)) )
			)
			
		)
	)


(define build-constants-table
	(lambda (exp)
		 (let* ((consts (get-constants-list '() exp))
		 		(splitted-consts (reverse (list->set (reverse (all-consts consts)))))
		 		)
		 	(create-records splitted-consts 0))
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

(define get-vector-refs
	(lambda (veclst lst table)
			(if (null? veclst) lst
				(get-vector-refs (cdr veclst) (append lst `(,(lookup-constant-get-label (car veclst) table))) table))
))
