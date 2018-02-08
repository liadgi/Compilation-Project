(define assembly-function-names
	(list 'cons)
)

(define init-functions
	(lambda (fvars)
		(set-cons (cons-label) (cons-end-label) fvars)
	))

(define set-cons
	(lambda (labelCons labelConsEnd fvars)
		(let ((label (lookup-fvar-get-label 'cons fvars)))
		(print-line "
					; function - cons
					mov rbx, SOB_NIL
					SAFE_MALLOC 16
					MAKE_LITERAL_CLOSURE rax, rbx, " labelCons"
					jmp " labelConsEnd"
					"labelCons":
					push rbp
					mov rbp, rsp

					mov rbx, [rbp+8*4] 		;car
					mov rcx, [rbp+8*5]		;cdr
					hi:
					SAFE_MALLOC 8
					MAKE_PAIR rbx, rcx
					mov [rax], rbx

					mov rbx, [rbp+8*3] 		;rbx <-- n
					leave
					pop rcx					;rcx <-- ret
					add rbx, 2
					shl rbx, 3
					add rsp, rbx 			;clean stack
					push rcx
					ret 
					"labelConsEnd":
					mov [" label "], rax
					mov rax, SOB_VOID
					"))
))

(define set-=
	3
	)


(define scheme-functions
	'(
		(define list (lambda x x))
		;(define list2 (lambda x (lambda (y) y)))
		;(define list2 (lambda (x y . z ) z))
		;(define complicated (lambda (x y . z) (if x (list y z (cons x z)) (list z y))))
		;(define zero? (lambda (x) (if (= x 0) #t #f)))
	)
)

(define scheme-expressions
	(map (lambda (e)
					(annotate-tc
						(pe->lex-pe
							(box-set
								(remove-applic-lambda-nil
									(parse e))))))

	scheme-functions
	))

(define display-newline
	(lambda (in)
		(display in)
		(newline)))

(define gen-global-functions
	(lambda (exps constants-table fvars)
		(map (lambda (exp) 
			(let ((function-name (symbol->string (cadadr exp))))
				(print-line ";start generating function \"" function-name "\"
					")
				
				(code-gen exp constants-table 0 fvars)

				(print-line ";end generating function \"" function-name "\"
					")
			)
			) exps)
		))



(define init-fvars-list
	(append assembly-function-names (map (lambda (x) (cadr x)) scheme-functions)))

(define get-fvars-list
	(lambda (exp) 
		(letrec ((run
			(lambda (var-list exp) 
				(if (not (pair? exp)) '()
					(if (eq? (car exp) 'fvar) (cdr exp)
						(append var-list (run var-list (car exp)) 
										 (run var-list (cdr exp))))
				)
			)
			))
		(list->set (run init-fvars-list exp))
		)
	)
)

(define label-fvars
	(lambda (labeled-fvars fvars)
		(if (null? fvars) labeled-fvars
			;(label-fvars (append labeled-fvars (list `(,(car fvars) ,(glob-label)))) (cdr fvars)))
			(label-fvars (append labeled-fvars (list `(,(car fvars) ,(string-append "Lglob_" (symbol->string (car fvars)))))) (cdr fvars)))
	))

(define lookup-fvar-get-label
	(lambda (fvar favrs-table)
		(if (eq? (caar favrs-table) fvar) (cadar favrs-table)
			(lookup-fvar-get-label fvar (cdr favrs-table)))
	))
