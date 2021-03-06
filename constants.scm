(define parse-list-helper
	(lambda (lst)
		(cond ((null? lst) lst)
			((number? lst) lst)
			;((basic? lst) lst)
			((list? lst) `(,@(parse-list-helper (cdr lst)) ,lst))

			)
))

(define parse-pair-helper 
	(lambda (lst)
		(cond ((basic? lst) lst)
			((basic? (car lst)) (append `(,(car lst)) (all-consts `(,(cdr lst))) `(,lst)))
			((vector? (car lst)) (append (all-consts `(,(car lst))) (all-consts `(,(cdr lst))) `(,lst)))
				
			((list? (car lst)) (append (all-consts (car lst)) (parse-list-helper (car lst)) `(,(parse-pair-helper (cdr lst))) `(,lst)))
			;((rational? (car lst)) (append (all-consts (numerator (car lst))) `(,(parse-list-helper (denominator (car lst)))) `(,(car lst))))
			)
))

(define basic? 
	(lambda (x)
		(or (null? x) (integer? x) (string? x) (boolean? x) (char? x))))

(define all-consts
	(lambda (lst)
		(cond ((basic? lst) lst)
			((basic? (car lst)) (append `(,(car lst)) (all-consts (cdr lst))))
			((list? (car lst)) (append (all-consts (car lst)) (parse-list-helper (car lst)) (all-consts (cdr lst)) ))
			((vector? (car lst)) (append (all-consts (vector->list (car lst))) `(,(car lst)) (all-consts (cdr lst))))
			((pair? (car lst)) (append (parse-pair-helper (car lst)) (all-consts (cdr lst))))
			((rational? (car lst)) (append `(,(numerator (car lst))) `(,(denominator (car lst))) `(,(car lst)) (all-consts (cdr lst))))
			((symbol? (car lst)) (append `(,(symbol->string (car lst))) `(,(car lst)) (all-consts (cdr lst))))
			)
))

(define create-records
	(lambda (constants-list counter)
			(letrec ((build 
				(lambda (lst table counter)
					(if (null? lst) table
						(if (or (boolean? (car lst)) (null? (car lst)) (and (number? (car lst)) (or (eq? (car lst) 0) (eq? (car lst) 1) (eq? (car lst) -1))))
							(build (cdr lst) table counter)
							(build (cdr lst) (append table 
								(list 
									(let* ((atom (car lst))
										   (label-counter (number->string counter)))
										(cond ;((boolean? atom) (list 'sob 'T_BOOL))
										  ((integer? atom) (list atom counter 'T_INTEGER (string-append "sobInt" (string-replace (number->string atom) "-" "minus") "_" label-counter)))
										  ((char? atom) (list atom counter 'T_CHAR (string-append "sobChar" label-counter)))
										  ((string? atom) (list atom counter 'T_STRING (string-append "sobString" label-counter)))
										  ;((number? atom) (list atom counter 'T_NUMBER (string-append "sobNumber" label-counter)))
										  ((pair? atom) 
										  	(let (
										  		  (first (lookup-constant-get-label (car atom) table))
											      (second (lookup-constant-get-label (cdr atom) table))
											   (label-counter (number->string counter)))
												`(,atom ,counter T_PAIR ,(string-append "sobPair" label-counter) ,first ,second)
												))
										  ((vector? atom) 
										  	(let ((refs (get-vector-refs (vector->list atom) '() table))
										  		(label-counter (number->string counter)))
										  		`(,atom ,counter T_VECTOR ,(string-append "sobVector" label-counter) ,@refs)))
										  ((rational? atom)
										  	(let ((numer (lookup-constant-get-label (numerator atom) table))
										  		(denom (lookup-constant-get-label (denominator atom) table))
										  		(label-counter (number->string counter)))
										  		`(,atom ,counter T_FRACTION ,(string-append "sobFraction" label-counter) ,numer ,denom)
										  	))
										  ((symbol? atom)
										  	(let ((str (lookup-constant-get-label (symbol->string atom) table))
											   		(label-counter (number->string counter))
											   		(next (update-last-symbol (string-append "sobSymbol" label-counter))))
												`(,atom ,counter T_SYMBOL ,(string-append "sobSymbol" label-counter) ,str ,next)))
										  (else (display-error "create-records: value" atom " does not fit any type"))
										)
								)))
							(+ counter 1))
							)
						))))
			(append (build (list->set constants-list) (list 
				(list (list) counter 'T_NIL "sobNil") 
				(list void (+ counter 1) 'T_VOID "sobVoid") 
				(list #t (+ counter 2) 'T_BOOL "sobTrue")
				(list #f (+ counter 3) 'T_BOOL "sobFalse")
				(list 0 (+ counter 4) 'T_INTEGER "sobInt0")
				(list 1 (+ counter 5) 'T_INTEGER "sobInt1")
				(list -1 (+ counter 6) 'T_INTEGER "sobIntMinus1")
				) (+ counter 7)) )
			)
			
		)
	)

(define next-symbol-label "sobNil")

(define update-last-symbol
	(lambda (label)
		(let ((prev next-symbol-label))
			(set! next-symbol-label label)
			prev)))

(define build-constants-table
	(lambda (exp)
		 (let* ((consts (get-constants-list '() exp))
		 		;(dbg (display-newline consts))
		 		(dismantled-consts (all-consts consts))
		 		(splitted-consts (reverse (list->set (reverse dismantled-consts))))
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
		(if (null? table) (begin (display "could not find constant ") (display-newline constant) (exit))
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
