(define shave (gensym))
(define assembly-function-names
	(list 
	'cons 
	'car ; returns new object
	'cdr ; returns new object
	'boolean? 'char? 'integer? 'null? 'pair? 'procedure? 'rational? 'string? 'symbol? 'vector? 
	; just without 'number?
	;'shave
	'=
	'<
	'>
	'not
	'char->integer
	'integer->char ; check this out
	;'string->symbol
	;'symbol->string
	'set-car! ; check this out
	'set-cdr! ; check this out
	'apply
	)
)

(define set-apply
	(lambda (labelApply labelApplyEnd fvars)
		(let ((label (lookup-fvar-get-label 'apply fvars))
			(looplabel (loop-label))
			(loopEndLabel (loop-end-label))
			(looplabel2 (loop-label))
			(loopEndLabel2 (loop-end-label)))
			(print-line (string-append "
					;*******	APPLY	*******
					mov rbx, SOB_NIL
					SAFE_MALLOC 16
					MAKE_LITERAL_CLOSURE rax, rbx, " labelApply"
					jmp " labelApplyEnd"
					"labelApply":
					push rbp
					mov rbp, rsp

					;count n
					mov rbx, qword[rbp+5*8]			
					mov rbx, [rbx]		;rbx <-- s (list of parameters)
					mov rcx, rbx		;rcx <-- backup to s
					mov rdi, 0
					"looplabel":
					cmp rbx, SOB_NIL
					je "loopEndLabel"					
					inc rdi
					CDR rbx
					jmp " looplabel"
					"loopEndLabel":

					;save old rbp
					mov r8, rbp 		;r8 <-- old rbp
					mov r11, [r8]
					mov rdx, rdi
					sub rdx, 2
					shl rdx, 3			; rdx <-- (n-2)*8 //how much to jump forward
					sub rbp, rdx 		;rbp <-- rbp-(n-2)*8

					;save f
					mov r10, qword[r8+(4*8)]

					;save old ret
					mov r9, qword[r8+(1*8)]	;previous ret
					mov [rbp+(1*8)], r9

					;get f - code & env
					mov r13, [r10]
					mov rbx, r13 	;rbx <-- closure(?)
					TYPE rbx
					cmp rbx, T_CLOSURE
					jne L_error_cannot_apply_non_clos
					mov rbx, r13
					CLOSURE_ENV rbx 	;save env
					mov [rbp+(2*8)], rbx
					CLOSURE_CODE r13

					;push n
					mov [rbp+(3*8)], rdi

					;push args from end (4*8) to begining ((3+n)*8)
					mov rdi, 4
					"looplabel2":
					cmp rcx, SOB_NIL
					je "loopEndLabel2"
					mov rbx, rcx
					CAR_ADDR rbx
					mov rdx, rdi
					shl rdx, 3
					mov [rbp+rdx], rbx					
					inc rdi
					CDR rcx
					jmp " looplabel2"
					"loopEndLabel2":

					"
					(debug-label)":
					;restore old rbp
					add rbp, 8
					mov rsp, rbp
					mov rbp, r11
					jmp r13
					
					"labelApplyEnd":
					mov [" label "], rax
					mov rax, SOB_VOID
					;*******	APPLY-END	*******
					"))
		))
	)


(define init-functions
	(lambda (fvars)
		(func-frame fvars "cons" impl-cons)
		(func-frame fvars "car" impl-car)
		(func-frame fvars "cdr" impl-cdr)
		(gen-predicates fvars)
		(func-frame fvars "rational?" impl-rational?)
		(func-frame fvars "=" impl-=)
		(func-frame fvars "<" impl-<)
		(func-frame fvars ">" impl->)
		(func-frame fvars "not" impl-not)
		(func-frame fvars "char->integer" impl-char->integer)
		(func-frame fvars "integer->char" impl-integer->char)
		(func-frame fvars "set-car!" impl-set-car!)
		(func-frame fvars "set-cdr!" impl-set-cdr!)
		(set-apply (apply-label) (apply-end-label) fvars)
	))


(define gen-func-prologue
	(lambda (name impl skip)
		(print-line "
			; start function - \""name"\"
			mov rbx, SOB_NIL
			SAFE_MALLOC 16
			MAKE_LITERAL_CLOSURE rax, rbx, " impl"
			jmp " skip"
			"impl":
			push rbp
			mov rbp, rsp
			"
		)))

(define gen-func-epilogue
	(lambda (name skip label)
		(print-line "
			mov rbx, [rbp+8*3] 		;rbx <-- n
			leave
			pop rcx					;rcx <-- ret
			add rbx, 2
			shl rbx, 3
			add rsp, rbx 			;clean stack
			push rcx
			ret 
			"skip":
			mov [" label "], rax
			mov rax, SOB_VOID
			; end function - \""name"\"
			"
		)))

(define clean-name
	(lambda (x)
		(<->less (>->greater (=->equals (dash->underline (remove-exclamation-mark (arrow->to x))))))
		))

(define <->less
	(lambda (x)
		(string-replace x "<" "less")))

(define >->greater
	(lambda (x)
		(string-replace x ">" "greater")))

(define =->equals
	(lambda (x)
		(string-replace x "=" "equals")))

(define remove-exclamation-mark
	(lambda (x)
		(string-replace x "!" "")))

(define dash->underline
	(lambda (x)
		(string-replace x "-" "_")))

(define arrow->to
	(lambda (x)
		(string-replace x "->" "_to_")))

(define func-frame
	(lambda (fvars name func)
		(let* ((cln-name (clean-name name))
			  (label (lookup-fvar-get-label (string->symbol name) fvars))
			  (impl (string-append cln-name "_label"))
			  (skip (string-append cln-name "_end_label")))
			(gen-func-prologue name impl skip)
			(print-line func)
			(gen-func-epilogue name skip label)
		)
))

(define impl-cons
	"
	mov rbx, [rbp+8*4] 		;car
	mov rcx, [rbp+8*5]		;cdr
	SAFE_MALLOC 8
	MAKE_PAIR rbx, rcx
	mov [rax], rbx
	"
	)

(define impl-car
"
	mov rbx, [rbp+8*4] 		; pair

	mov rax, [rbx]
	TYPE rax
	cmp qword rax, T_PAIR
	je car_type_ok
	jmp exit_compiler
	car_type_ok:

	mov rbx, [rbx]
	CAR rbx
	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rbx
	")

(define impl-cdr
"
	mov rbx, [rbp+8*4] 		; pair

	mov rax, [rbx]
	TYPE rax
	cmp qword rax, T_PAIR
	je cdr_type_ok
	jmp exit_compiler
	cdr_type_ok:

	mov rbx, [rbx]
	CDR rbx
	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rbx
	")

(define impl-rational? ; fraction or integer
"
	mov rbx, [rbp+8*4] 		; pair

	mov rax, [rbx]
	TYPE rax
	cmp qword rax, T_FRACTION
	je indeed_rational
	cmp qword rax, T_INTEGER
	je indeed_rational
	mov rax, sobFalse
	jmp end_rational?
	indeed_rational:
	mov rax, sobTrue

	end_rational?:
	")

(define predicate-impl
	(lambda (type)
		(string-append
		"
		mov rbx, [rbp+8*4]
		mov rax, [rbx]
		TYPE rax
		cmp qword rax, "type"
		je indeed_" type "
		mov rax, sobFalse
		jmp end_" type "?
		indeed_" type ":
		mov rax, sobTrue

		end_" type "?:
	")
))

(define gen-predicates
	(lambda (fvars)
			(func-frame fvars "boolean?" (predicate-impl "T_BOOL"))
			(func-frame fvars "char?" (predicate-impl "T_CHAR"))
			(func-frame fvars "integer?" (predicate-impl "T_INTEGER"))
			(func-frame fvars "null?" (predicate-impl "T_NIL"))
			;(func-frame fvars "number?" (predicate-impl "T_NUMBER"))
			(func-frame fvars "pair?" (predicate-impl "T_PAIR"))
			(func-frame fvars "procedure?" (predicate-impl "T_CLOSURE"))
			(func-frame fvars "string?" (predicate-impl "T_STRING"))
			(func-frame fvars "symbol?" (predicate-impl "T_SYMBOL"))
			(func-frame fvars "vector?" (predicate-impl "T_VECTOR"))

		))


(define impl-=
	"
	mov rbx, [rbp+8*3] ; n
	cmp rbx, 1
	jg shave_multiple_params
	cmp rbx, 0
	je exit_compiler
	mov rbx, [rbp+8*4] ; first param 	
	mov rbx, [rbx]
	TYPE rbx

	cmp qword rbx, T_INTEGER
	je shave_param_ok
	cmp qword rbx, T_FRACTION
	je shave_param_ok
	jmp exit_compiler

	shave_param_ok:
	mov rax, sobTrue
	jmp endShave

	shave_multiple_params:


	mov r10, [rbp+8*3] ; n
	dec r10
	mov rdi, -1
	shave_compare:
	inc rdi
	cmp rdi, r10 
	je endShave

	mov rbx, [rbp+8*(rdi+4)] 
	mov rcx, [rbp+8*(rdi+5)]

	mov rbx, [rbx]
	mov rcx, [rcx]
	TYPE rbx
	TYPE rcx
	cmp qword rbx, rcx
	jne not_shave
	cmp qword rbx, T_INTEGER
	je compare_integers
	cmp qword rbx, T_FRACTION
	je compare_fractions
	jmp exit_compiler
	compare_fractions:
	mov rbx, [rbp+8*(rdi+4)] ; first param 	
	mov rcx, [rbp+8*(rdi+5)] ; second param
	mov rbx, [rbx]
	mov rcx, [rcx]
	CAR rbx ; numer
	CAR rcx ; numer
	cmp rbx, rcx
	jne not_shave
	mov rbx, [rbp+8*(rdi+4)] ; first param 	
	mov rcx, [rbp+8*(rdi+5)] ; second param
	mov rbx, [rbx]
	mov rcx, [rcx]
	CDR rbx ; denom
	CDR rcx ; denom
	cmp rbx, rcx
	jne not_shave
	mov rax, sobTrue
	jmp shave_compare

	compare_integers:
	mov rbx, [rbp+8*(rdi+4)] ; first param 	
	mov rcx, [rbp+8*(rdi+5)] ; second param
	mov rbx, [rbx]
	mov rcx, [rcx]
	DATA rbx
	DATA rcx
	cmp qword rbx, rcx
	jne not_shave
	mov rax, sobTrue
	jmp shave_compare

	not_shave:
	mov rax, sobFalse

	endShave:
	"
	)

(define impl-<
	"
	mov rbx, [rbp+8*3] ; n
	cmp rbx, 1
	jg lessthan_multiple_params
	cmp rbx, 0
	je exit_compiler
	mov rbx, [rbp+8*4] ; first param 	
	mov rbx, [rbx]
	TYPE rbx

	cmp qword rbx, T_INTEGER
	je lessthan_param_ok
	cmp qword rbx, T_FRACTION
	je lessthan_param_ok
	jmp exit_compiler

	lessthan_param_ok:
	mov rax, sobTrue
	jmp endlessthan

	lessthan_multiple_params:


	mov r10, [rbp+8*3] ; n
	dec r10
	mov rdi, -1
	lessthan_compare:
	inc rdi
	cmp rdi, r10 
	je endlessthan

	mov rbx, [rbp+8*(rdi+4)] 
	mov rcx, [rbp+8*(rdi+5)]

	mov rbx, [rbx]
	mov rcx, [rcx]
	TYPE rbx
	TYPE rcx
	cmp qword rbx, rcx
	jne not_lessthan
	cmp qword rbx, T_INTEGER
	je lessthan_compare_integers
	cmp qword rbx, T_FRACTION
	je lessthan_compare_fractions
	jmp exit_compiler
	lessthan_compare_fractions:
	;mov rbx, [rbp+8*(rdi+4)] ; first param 	
	;mov rcx, [rbp+8*(rdi+5)] ; second param
	;mov rbx, [rbx]
	;mov rcx, [rcx]
	;CAR rbx ; numer
	;CAR rcx ; numer
	;cmp rbx, rcx
	;jne not_lessthan
	;mov rbx, [rbp+8*(rdi+4)] ; first param 	
	;mov rcx, [rbp+8*(rdi+5)] ; second param
	;mov rbx, [rbx]
	;mov rcx, [rcx]
	;CDR rbx ; denom
	;CDR rcx ; denom
	;cmp rbx, rcx
	;jne not_lessthan
	;mov rax, sobTrue
	;jmp lessthan_compare

	lessthan_compare_integers:
	mov rbx, [rbp+8*(rdi+4)] ; first param 	
	mov rcx, [rbp+8*(rdi+5)] ; second param
	mov rbx, [rbx]
	mov rcx, [rcx]
	DATA rbx
	DATA rcx
	cmp qword rbx, rcx
	jge not_lessthan
	mov rax, sobTrue
	jmp lessthan_compare

	not_lessthan:
	mov rax, sobFalse

	endlessthan:
	"
	)

(define impl->
	"
	mov rbx, [rbp+8*3] ; n
	cmp rbx, 1
	jg greaterthan_multiple_params
	cmp rbx, 0
	je exit_compiler
	mov rbx, [rbp+8*4] ; first param 	
	mov rbx, [rbx]
	TYPE rbx

	cmp qword rbx, T_INTEGER
	je greaterthan_param_ok
	cmp qword rbx, T_FRACTION
	je greaterthan_param_ok
	jmp exit_compiler

	greaterthan_param_ok:
	mov rax, sobTrue
	jmp endgreaterthan

	greaterthan_multiple_params:


	mov r10, [rbp+8*3] ; n
	dec r10
	mov rdi, -1
	greaterthan_compare:
	inc rdi
	cmp rdi, r10 
	je endgreaterthan

	mov rbx, [rbp+8*(rdi+4)] 
	mov rcx, [rbp+8*(rdi+5)]

	mov rbx, [rbx]
	mov rcx, [rcx]
	TYPE rbx
	TYPE rcx
	cmp qword rbx, rcx
	jne not_greaterthan
	cmp qword rbx, T_INTEGER
	je greaterthan_compare_integers
	cmp qword rbx, T_FRACTION
	je greaterthan_compare_fractions
	jmp exit_compiler
	greaterthan_compare_fractions:
	;mov rbx, [rbp+8*(rdi+4)] ; first param 	
	;mov rcx, [rbp+8*(rdi+5)] ; second param
	;mov rbx, [rbx]
	;mov rcx, [rcx]
	;CAR rbx ; numer
	;CAR rcx ; numer
	;cmp rbx, rcx
	;jne not_greaterthan
	;mov rbx, [rbp+8*(rdi+4)] ; first param 	
	;mov rcx, [rbp+8*(rdi+5)] ; second param
	;mov rbx, [rbx]
	;mov rcx, [rcx]
	;CDR rbx ; denom
	;CDR rcx ; denom
	;cmp rbx, rcx
	;jne not_greaterthan
	;mov rax, sobTrue
	;jmp greaterthan_compare

	greaterthan_compare_integers:
	mov rbx, [rbp+8*(rdi+4)] ; first param 	
	mov rcx, [rbp+8*(rdi+5)] ; second param
	mov rbx, [rbx]
	mov rcx, [rcx]
	DATA rbx
	DATA rcx
	cmp qword rbx, rcx
	jle not_greaterthan
	mov rax, sobTrue
	jmp greaterthan_compare

	not_greaterthan:
	mov rax, sobFalse

	endgreaterthan:
	"
	)

(define impl-not
"
	mov rbx, [rbp+8*3] ; n
	cmp rbx, 1
	jne exit_compiler

	mov rbx, [rbp+8*4] 		; the single param
	mov rbx, [rbx]
	mov rax, sobFalse
	mov rax, [rax]
	cmp qword rax, rbx
	jne not_set_false

	mov rax, sobTrue
	jmp not_end

	not_set_false:
	mov rax, sobFalse

	not_end:
	"
	)

(define impl-char->integer
"
	mov rbx, [rbp+8*3] ; n
	cmp rbx, 1
	jne exit_compiler

	mov rbx, [rbp+8*4] 		; the single param
	mov rbx, [rbx]
	TYPE rbx
	cmp qword rbx, T_CHAR
	jne exit_compiler

	mov rbx, [rbp+8*4] 		; the single param
	mov rbx, [rbx]
	sar rbx, TYPE_BITS
	sal rbx, TYPE_BITS
	or rbx, T_INTEGER
	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rbx

	"
	)
(define impl-integer->char
	"
	mov rbx, [rbp+8*3] ; n
	cmp rbx, 1
	jne exit_compiler

	mov rbx, [rbp+8*4] 		; the single param
	mov rbx, [rbx]
	TYPE rbx
	cmp qword rbx, T_INTEGER
	jne exit_compiler

	mov rbx, [rbp+8*4] 		; the single param
	mov rbx, [rbx]
	sar rbx, TYPE_BITS
	sal rbx, TYPE_BITS
	or rbx, T_CHAR
	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rbx

	"
	)

(define impl-set-car!
	"
	mov rbx, [rbp+8*3] ; n
	cmp rbx, 2
	jne exit_compiler

	mov rbx, [rbp+8*4] 		; the first param

	mov rbx, [rbx]
	TYPE rbx
	cmp qword rbx, T_PAIR
	jne exit_compiler

	mov rbx, [rbp+8*4] 		; the first param
	mov rbx, [rbx]
	mov rcx, [rbp+8*5] 		; the second param

	shl rbx, ((WORD_SIZE - TYPE_BITS) >> 1) ; move 30 bits left
	shr rbx, ((WORD_SIZE - TYPE_BITS) >> 1) ; move 30 bits right

	sub rcx, start_of_data
	shl rcx, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS) ; move 34 bits left
	or rbx, rcx

	mov rcx, [rbp+8*4]		; the first param
	mov qword [rcx], rbx
	mov rax, sobVoid

	"
	)

(define impl-set-cdr!
	"
	mov rbx, [rbp+8*3] ; n
	cmp rbx, 2
	jne exit_compiler

	mov rbx, [rbp+8*4] 		; the first param

	mov rbx, [rbx]
	TYPE rbx
	cmp qword rbx, T_PAIR
	jne exit_compiler

	mov rbx, [rbp+8*4] 		; the first param
	mov rbx, [rbx]
	mov rcx, [rbp+8*5] 		; the second param

	shr rbx, (((WORD_SIZE - TYPE_BITS) >> 1)+ TYPE_BITS) ; move 34 bits right
	shl rbx, (((WORD_SIZE - TYPE_BITS) >> 1)+ TYPE_BITS) ; move 34 bits left

	sub rcx, start_of_data
	shl rcx, TYPE_BITS ; move 4 bits left
	or rbx, T_PAIR
	or rbx, rcx

	mov rcx, [rbp+8*4]		; the first param
	mov qword [rcx], rbx
	mov rax, sobVoid

	"
	)


(define scheme-functions
	'(
		(define list (lambda x x))
		(define number? (lambda (x) (rational? x)))
		#;(define = 
			(lambda x
				(let ((first (car x)) (rest (cdr x)))
					(letrec ((run (lambda y
									(let* ((lst (car y))
										   (first (car lst))
										   (rest (cdr lst)))
									(cond ((and (null? rest) (number? first)) (shave first))
							  		(else
							  			(cond ((null? (cdr rest)) (shave first (car rest))) 
							  			  (else (and (shave first (car rest))
							  			  			 (run rest)))
							  		)
							  )
							)
										))))
				(cond ((and (null? rest) (number? first)) (shave first)) 
						  		(else
						  			(cond ((null? (cdr rest)) (shave first (car rest)))
						  			  (else (and (shave first (car rest))
						  			  			 (run rest)))
						  		)
						  )
						)

					))
			)
		)
		(define zero? (lambda (x) (= x 0)))
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

(define display-error
	(lambda params
			(display-newline params)
			(exit))
)

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

(define prefix?
	(lambda (haystack needle)
		(cond ((and (null? haystack) (null? needle)) #t)
			  ((and (null? haystack) (not (null? needle))) #f)
			  ((and (null? needle) (not (null? haystack))) #t)
			  ((not (eq? (car haystack) (car needle))) #f)
			  (else (prefix? (cdr haystack) (cdr needle)))
			)
))

(define reverse-append
	(lambda (a b)
		(append (reverse a) b)
		))


(define (string-replace haystack needle replacement)    
  ;; most of the processing works on lists 
  ;; of char, not strings.
  (let ((haystack (string->list haystack))
        (needle (string->list needle))
        (replacement (string->list replacement))
        (needle-len (string-length needle)))
    (let loop ((haystack haystack) (acc '()))
      (cond ((null? haystack)
             (list->string (reverse acc)))
            ((prefix? haystack needle)
             (loop (list-tail haystack needle-len)
                   (reverse-append replacement acc)))
            (else
             (loop (cdr haystack) (cons (car haystack) acc)))))))

(define init-fvars-list
	(append assembly-function-names (map (lambda (x) (cadr x)) scheme-functions)))

(define get-fvars-list
	(lambda (exp) 
		(letrec ((run
			(lambda (var-list exp) 
				(if (not (pair? exp)) var-list
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
			(label-fvars (append labeled-fvars (list `(,(car fvars) ,(string-append "Lglob_" (clean-name (symbol->string (car fvars))))))) (cdr fvars)))
	))

(define lookup-fvar-get-label
	(lambda (fvar favrs-table)
		(if (eq? (caar favrs-table) fvar) (cadar favrs-table)
			(lookup-fvar-get-label fvar (cdr favrs-table)))
	))
