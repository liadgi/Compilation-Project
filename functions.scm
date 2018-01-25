(define init-functions
	(lambda (fvars)
		(set-cons (cons-label) (cons-end-label) fvars)
	))

(define set-cons
	(lambda (labelCons labelConsEnd fvars)
		(let ((label (lookup-fvar-get-label 'cons fvars)))
		(print-line (string-append "
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
					add rbx, 3
					shl rbx, 3
					add rsp, rbx 			;clean stack
					push rcx
					ret 
					"labelConsEnd":
					mov [" label "], rax
					mov rax, SOB_VOID
					")))
))

(define scheme-expressions
	(map (lambda (e)
					(annotate-tc
						(pe->lex-pe
							(box-set
								(remove-applic-lambda-nil
									(parse e))))))

	'(
		(define list (lambda x x))
	)
	))