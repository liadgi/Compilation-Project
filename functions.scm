(define assembly-function-names
	(list 
	'cons 
	'car ; returns new object
	'cdr ; returns new object
	'boolean? 'char? 'integer? 'null? 'pair? 'procedure? 'rational? 'string? 'symbol? 'vector? 
	; just without 'number?
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

	'numerator
	'denominator
	'+
	'-
	'*
	'/
	)
)


(define init-functions
	(lambda (fvars)
		(func-frame fvars "cons" impl-cons)
		(func-frame fvars "car" impl-car)
		(func-frame fvars "cdr" impl-cdr)
		(gen-predicates fvars)
		(func-frame fvars "rational?" impl-rational?)
		(gen-comparisons fvars)
		(func-frame fvars "not" impl-not)
		(func-frame fvars "char->integer" impl-char->integer)
		(func-frame fvars "integer->char" impl-integer->char)
		(func-frame fvars "set-car!" impl-set-car!)
		(func-frame fvars "set-cdr!" impl-set-cdr!)
		(set-apply (apply-label) (apply-end-label) fvars)
		(func-frame fvars "numerator" impl-numerator)
		(func-frame fvars "denominator" impl-denominator)
		(gen-arithmetics fvars)
	))


(define impl-integer->string
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
	or rbx, T_STRING
	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rbx
	")

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
		(/->divide (*->multiply (-->minus (+->plus
			(<->less (>->greater (=->equals 
				(dash->underline (remove-exclamation-mark (arrow->to x))))))))))
		))


(define +->plus
	(lambda (x)
		(string-replace x "+" "plus")))

(define -->minus
	(lambda (x)
		(string-replace x "-" "minus")))

(define *->multiply
	(lambda (x)
		(string-replace x "*" "multiply")))

(define /->divide
	(lambda (x)
		(string-replace x "/" "divide")))

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

	mov rax, [rbx]
	CAR_ADDR rax
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

	mov rax, [rbx]
	CDR_ADDR rax
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



(define gen-comparisons
	(lambda (fvars)
		3
		(func-frame fvars ">" (comparison-impl "greaterthan" "jle"))
		(func-frame fvars "<" (comparison-impl "lessthan" "jge"))
		(func-frame fvars "=" (comparison-impl "equals" "jne"))
))

(define comparison-impl
	(lambda (sign jmpInstruction)
		(string-append
		"
		mov rbx, [rbp+8*3] ; n
		cmp rbx, 1
		jg "sign"_multiple_params
		cmp rbx, 0
		je exit_compiler
		mov rbx, [rbp+8*4] ; first param 	
		mov rbx, [rbx]
		TYPE rbx

		cmp qword rbx, T_INTEGER
		je "sign"_param_ok
		cmp qword rbx, T_FRACTION
		je "sign"_param_ok
		jmp exit_compiler

		"sign"_param_ok:
		mov rax, sobTrue
		jmp end"sign"

		"sign"_multiple_params: ; assuming all is integer or fraction


		mov r10, [rbp+8*3] ; n
		dec r10
		mov rdi, -1

		"sign"_compare:
		inc rdi
		cmp rdi, r10 
		je end"sign"

		mov rbx, [rbp+8*(rdi+4)] 
		mov rcx, [rbp+8*(rdi+5)]
		push rbx
		push rcx
		call set_params_to_fractions ; rbx = (possibly new [rbp+8*(rdi+4)]) , rcx = (possibly new [rbp+8*(rdi+5)])
		pop rax ; just balance stack
		pop rax ; just balance stack
		
		; set the params back on the stack
		mov [rbp+8*(rdi+4)], rcx ; rcx is first
		mov [rbp+8*(rdi+5)], rbx ; rbx is second

		; decide if compare integers or fractions
		mov rbx, [rbp+8*(rdi+4)] ; first param 	
		mov rcx, [rbp+8*(rdi+5)] ; second param
		mov rbx, [rbx]
		mov rcx, [rcx]
		TYPE rbx
		TYPE rcx
		cmp qword rbx, rcx
		jne "sign"_compare_fractions ;; not checking if not fraction/integer
		cmp qword rbx, T_INTEGER
		je "sign"_compare_integers
		cmp qword rbx, T_FRACTION
		je "sign"_compare_fractions
		jmp exit_compiler


		"sign"_compare_fractions:

		; first fraction
		mov rbx, [rbp+8*(rdi+4)] ; first param 	
		mov rcx, [rbp+8*(rdi+5)] ; second param
		mov rbx, [rbx]
		mov rcx, [rcx]
		CAR rbx ; 1st numer
		CDR rcx ; 2nd denom
		DATA rbx
		DATA rcx
		mov rax, rbx
		imul rcx ; res in rdx:rax
		mov r8, rax ; 

		; second fraction
		mov rbx, [rbp+8*(rdi+4)] ; first param 	
		mov rcx, [rbp+8*(rdi+5)] ; second param
		mov rbx, [rbx]
		mov rcx, [rcx]
		CAR rcx ; 2nd numer
		CDR rbx ; 1st denom
		DATA rbx
		DATA rcx
		mov rax, rbx
		imul rcx ; res in rdx:rax
		mov r9, rax ; ?

		cmp r8, r9
		"jmpInstruction" not_"sign"
		mov rax, sobTrue
		jmp "sign"_compare

		"sign"_compare_integers:
		mov rbx, [rbp+8*(rdi+4)] ; first param 	
		mov rcx, [rbp+8*(rdi+5)] ; second param
		mov rbx, [rbx]
		mov rcx, [rcx]
		DATA rbx
		DATA rcx
		cmp qword rbx, rcx
		"jmpInstruction" not_"sign"
		mov rax, sobTrue
		jmp "sign"_compare

		not_"sign":
		mov rax, sobFalse

		end"sign":
	")
		))



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
	sar rbx, ((WORD_SIZE - TYPE_BITS) >> 1) ; move 30 bits right

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

	sar rbx, (((WORD_SIZE - TYPE_BITS) >> 1)+ TYPE_BITS) ; move 34 bits right
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

(define impl-numerator
	"
	mov rbx, [rbp+8*3] ; n
	cmp rbx, 1
	jne exit_compiler

	mov rbx, [rbp+8*4] 		; the first param

	mov rbx, [rbx]
	TYPE rbx
	cmp qword rbx, T_INTEGER
	je numerator_integer
	cmp qword rbx, T_FRACTION
	je numerator_fraction
	jne exit_compiler

	numerator_integer:
	mov rax, [rbp+8*4] 		; the value itself
	jmp end_numerator

	numerator_fraction:
	mov rax, [rbp+8*4] 		; the first param
	mov rax, [rax] ; T_FRACTION

	sar rax, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS) ; move 34 bits right
	add rax, start_of_data

	jmp end_numerator

	end_numerator:
	"
)

(define impl-denominator
	"
	mov rbx, [rbp+8*3] ; n
	cmp rbx, 1
	jne exit_compiler

	mov rbx, [rbp+8*4] 		; the first param

	mov rbx, [rbx]
	TYPE rbx
	cmp qword rbx, T_INTEGER
	je denominator_integer
	cmp qword rbx, T_FRACTION
	je denominator_fraction
	jne exit_compiler

	denominator_integer:
	mov rax, sobInt1 		; the value 1
	jmp end_denominator

	denominator_fraction:
	mov rax, [rbp+8*4] 		; the first param
	mov rax, [rax] ; T_FRACTION

	shl rax, ((WORD_SIZE - TYPE_BITS) >> 1) ; move 30 bits left
	sar rax, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS) ; move 34 bits right
	add rax, start_of_data
	jmp end_denominator

	end_denominator:
	"
)



(define gen-arithmetics
	(lambda (fvars)
		(func-frame fvars "+" (impl-arithmetic "plus" +_no_param-impl +_single_param-impl +_multiple_params_integers-impl (+-multiple_params_fractions-gen "plus" +_multiple_params_fractions_action) ""))
		(func-frame fvars "-" (impl-arithmetic "minus" -_no_param-impl -_single_param-impl -_multiple_params_integers-impl (+-multiple_params_fractions-gen "minus" -_multiple_params_fractions_action) ""))
		(func-frame fvars "*" (impl-arithmetic "multiply" *_no_param-impl *_single_param-impl *_multiple_params_integers-impl (*/multiple_params_fractions-gen *_multiple_params_fractions_action) (check_zero_loop "multiply" *_check_zero_loop)))
		(func-frame fvars "/" (impl-arithmetic "divide" /_no_param-impl /_single_param-impl /_multiple_params_integers-impl (*/multiple_params_fractions-gen /_multiple_params_fractions_action) (check_zero_loop "divide" /_check_zero_loop)))
))




(define +_no_param-impl
	"; return 0
		mov rax, sobInt0")
(define +_single_param-impl "mov rax, [rbp+8*4] ; first param")
(define +_multiple_params_integers-impl "
	add rbx, rcx
	shl rbx, TYPE_BITS
	or rbx, T_INTEGER
	")
(define +_multiple_params_fractions_action "
	add r8, r9 ; r8 = (1st numer * 2nd denom) + (2nd numer * 1st denom)"
)



(define -_no_param-impl " jmp exit_compiler ")
(define -_single_param-impl " ; TODO
	; the result = 0 minus first param
	mov rax, [rbp+8*4] ; fraction address
	mov rax, [rax] ; T_FRACTION
	mov rbx, rax ; T_FRACTION

	mov rcx, 0

	TYPE rax
	cmp rax, T_INTEGER
	je minus_single_param_integer
	cmp rax, T_FRACTION
	je minus_single_param_fraction
	jmp exit_compiler

	minus_single_param_fraction:
	CAR rbx ; numerator (T_INTEGER)
	DATA rbx
	sub rcx, rbx ; rcx = 0 - rbx , numerator
	sal rcx, TYPE_BITS
	or rcx, T_INTEGER

	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rcx 
	mov rcx, rax ; rcx = new (negative) numerator, REAL address
	sub rcx, start_of_data ; new (negative) numerator, RELATIVE address

	mov rbx, [rbp+8*4] ; fraction address
	mov rbx, [rbx] ; T_FRACTION
	DATA_LOWER rbx ; rbx = denominator RELATIVE address
	sal rbx, TYPE_BITS
	mov rdx, rcx
	sal rdx, 34
	or rdx, rbx
	or rdx, T_FRACTION
	mov rbx, rdx

	jmp end_minus_single_param
	minus_single_param_integer:
	DATA rbx
	sub rcx, rbx ; rcx = 0 - rbx
	mov rbx, rcx
	sal rbx, TYPE_BITS
	or rbx, T_INTEGER
	
	end_minus_single_param:
	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rbx

	") 
(define -_multiple_params_integers-impl "
	sub rbx, rcx
	shl rbx, TYPE_BITS
	or rbx, T_INTEGER
	")
(define -_multiple_params_fractions_action
	"sub r8, r9 ; r8 = (1st numer * 2nd denom) + (2nd numer * 1st denom)"
)


(define +-multiple_params_fractions-gen
	(lambda (sign action)
		(string-append 
		 "
			; first fraction
			mov rbx, [rbp+8*(rdi+4)] ; first param 	
			mov rcx, [rbp+8*(rdi+5)] ; second param
			mov rbx, [rbx]
			mov rcx, [rcx]
			CAR rbx ; 1st numer
			CDR rcx ; 2nd denom
			DATA rbx
			DATA rcx
			mov rax, rbx
			imul rcx ; res in rdx:rax
			mov r8, rax ; r8 = 1st numer * 2nd denom

			; second fraction
			mov rbx, [rbp+8*(rdi+4)] ; first param 	
			mov rcx, [rbp+8*(rdi+5)] ; second param
			mov rbx, [rbx]
			mov rcx, [rcx]
			CAR rcx ; 2nd numer
			CDR rbx ; 1st denom
			DATA rbx
			DATA rcx
			mov rax, rbx
			imul rcx ; res in rdx:rax
			mov r9, rax ; r9 = 2nd numer * 1st denom

			; addition or subtraction of new numerators
			" 
			action 
			"
			cmp r8, 0
			jne "sign"_continue
			mov rax, sobInt0
			jmp "sign"_endloop
			"sign"_continue:

			; denominator
			mov rbx, [rbp+8*(rdi+4)] ; first param 	
			mov rcx, [rbp+8*(rdi+5)] ; second param
			mov rbx, [rbx]
			mov rcx, [rcx]
			CDR rbx ; 1st denom
			CDR rcx ; 2nd denom
			DATA rbx
			DATA rcx
			mov rax, rbx
			imul rcx ; res in rdx:rax
			mov r10, rax ; r10 = 1st denom * 2nd denom
			")))


(define *_no_param-impl
	"; return 1
		mov rax, sobInt1")
(define *_single_param-impl "mov rax, [rbp+8*4] ; first param")
(define *_multiple_params_integers-impl "
	;rbx = 1st, rcx = 2nd
	mov rax, rbx
	imul rcx ; res in rdx:rax. rcx * rax
	mov rbx, rax

	shl rbx, TYPE_BITS
	or rbx, T_INTEGER
	; res in rbx
	")

(define /_multiple_params_integers-impl " ; FIX THIS
	;rbx = 1st, rcx = 2nd

	cmp rbx, 0
	jl div_first_int_is_neg
	cmp rcx, 0
	jl div_second_int_is_neg

	div_first_int_is_neg:
	cmp rcx, 0
	jl div_second_int_is_neg
	jmp div_int_resume

	div_second_int_is_neg:

	mov rax, 0
	sub rax, rbx
	mov rbx, rax

	mov rax, 0
	sub rax, rcx
	mov rcx, rax

	div_int_resume:

	; save first
	sal rbx, TYPE_BITS
	or rbx, T_INTEGER
	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rbx
	sub rax, start_of_data
	sal rax, 34
	mov rbx, rax 

	; save second
	sal rcx, TYPE_BITS
	or rcx, T_INTEGER
	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rcx 
	sub rax, start_of_data
	sal rax, 4

	or rax, T_FRACTION
	or rbx, rax
	; res in rbx
	")

(define *_multiple_params_fractions_action "
		; rax = 1st numer
		; rbx = 2nd numer
		; rcx = 1st denom
		; rdx = 2nd denom
		mov r13, rdx

		; put in rax
		imul rbx ; res in rdx:rax. rax * rbx
		cmp rax, 0
		jne mul_not_zero

		mov rax, sobInt0
		pop r10
		jmp multiply_loop

		mul_not_zero:
		mov r8, rax

		mov rdx, r13
		; put in rax
		mov rax, rcx
		imul rdx ; res in rdx:rax. rcx * rdx
		mov r10, rax

	")





(define /_no_param-impl " jmp exit_compiler ")
(define /_single_param-impl "
	mov rax, [rbp+8*4] ; param address
	mov rbx, [rax]
	TYPE rbx
	cmp rbx, T_INTEGER ; is it integer?
	jne divide_single_param_not_integer

	; integer
	mov rbx, [rax] ; T_INTEGER
	DATA rbx
	cmp rbx, 0
	je exit_compiler
	; integer not 0
	cmp rbx, 0
	jl divide_single_param_integer_negative
	; positive

	mov rcx, sobInt1
	sub rcx, start_of_data
	sal rcx, 34
	mov rbx, [rax] ; T_INTEGER

	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rbx ; T_INTEGER

	sub rax, start_of_data
	sal rax, TYPE_BITS
	or rax, T_FRACTION
	or rcx, rax

	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rcx ; T_FRACTION

	jmp end_divide_single_param

	divide_single_param_integer_negative:
	mov rcx, sobIntMinus1
	sub rcx, start_of_data
	sal rcx, 34
	mov rax, [rax] ; T_INTEGER
	DATA rax
	mov rbx, 0
	sub rbx, rax
	sal rbx, TYPE_BITS
	or rbx, T_INTEGER ; now its positive

	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rbx ; T_INTEGER

	sub rax, start_of_data
	sal rax, TYPE_BITS
	or rax, T_FRACTION
	or rcx, rax

	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rcx ; T_FRACTION

	jmp end_divide_single_param


	divide_single_param_not_integer: ; is it fraction?
	cmp rbx, T_FRACTION
	jne exit_compiler

	; fraction

	mov rax, [rbp+8*4] ; param address
	mov rbx, [rax] ; T_FRACTION
	CAR rbx
	DATA rbx
	cmp rbx, 0
	je exit_compiler
	; integer not 0
	cmp rbx, 0
	jl divide_single_param_fraction_negative
	; positive - just flip

	mov rax, [rax] ; T_FRACTION
	mov rbx, rax
	DATA_LOWER rbx
	sal rbx, 34
	mov rcx, rax
	DATA_UPPER rcx
	sal rcx, 4
	or rcx, T_FRACTION
	or rbx, rcx

	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rbx ; T_FRACTION

	jmp end_divide_single_param

	divide_single_param_fraction_negative:
	mov rax, [rbp+8*4] ; param address
	mov rbx, [rax] ; T_FRACTION

	; denominator
	CDR rbx
	DATA rbx
	mov rdx, 0
	sub rdx, rbx
	sal rdx, TYPE_BITS
	or rdx, T_INTEGER

	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rdx ; T_INTEGER
	mov rdx, rax
	sub rdx, start_of_data
	sal rdx, 34

	; numerator
	mov rax, [rbp+8*4] ; param address
	mov rbx, [rax] ; T_FRACTION
	CAR rbx
	DATA rbx
	mov rcx, 0
	sub rcx, rbx
	sal rcx, TYPE_BITS
	or rcx, T_INTEGER

	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rcx ; T_INTEGER
	mov rcx, rax
	sub rcx, start_of_data
	sal rcx, TYPE_BITS
	or rcx, T_FRACTION
	or rdx, rcx

	SAFE_MALLOC 8 ; rax = SAFE_MALLOC
	mov [rax], rdx ; T_FRACTION

	end_divide_single_param:
	") 



	

		
(define /_multiple_params_fractions_action "
		; rax = 1st numer
		; rbx = 2nd numer
		; rcx = 1st denom
		; rdx = 2nd denom

		mov r13, rdx

		; put in rax
		imul rdx ; res in rdx:rax. rax * rdx
		;imul rbx ; res in rdx:rax. rax * rbx
		cmp rax, 0
		jne div_not_zero

		mov rax, sobInt0
		pop r10
		jmp divide_loop

		div_not_zero:
		mov r8, rax

		mov rdx, r13
		; put in rax
		mov rax, rcx
		imul rbx ; res in rdx:rax. rbx * rcx
		;imul rdx ; res in rdx:rax. rcx * rdx
		mov r10, rax
	")





(define */multiple_params_fractions-gen
	(lambda (action) 
		(string-append
		 "
			mov rax, [rbp+8*(rdi+4)] ; first fraction
			mov rax, [rax]
			CAR rax ; rax = 1st numer
			DATA rax

			mov rbx, [rbp+8*(rdi+5)] ; second fraction
			mov rbx, [rbx]
			CAR rbx ; rbx = 2nd numer			
			DATA rbx

			mov rcx, [rbp+8*(rdi+4)] ; first fraction
			mov rcx, [rcx]
			CDR rcx ; rcx = 1st denom
			DATA rcx

			mov rdx, [rbp+8*(rdi+5)] ; second fraction
			mov rdx, [rdx]
			CDR rdx ; rdx = 2nd denom
			DATA rdx

			; multiply or divide new numerators. r8 - numer, r10 - denom
			"action"

			
			")))

(define *_check_zero_loop "
	mov rax, sobInt0
	jmp endmultiply
	")
(define /_check_zero_loop "jmp exit_compiler")

(define check_zero_loop
	(lambda (sign action)
		(string-append
			"
			mov r10, [rbp+8*3] ; n
			dec r10
			mov rdi, -1

			"sign"_zero_loop:
			inc rdi
			cmp rdi, r10 
			je "sign"_end_zero_loop

			mov rbx, [rbp+8*(rdi+4)]
			mov rbx, [rbx]
			mov rax, rbx
			TYPE rax
			cmp rax, T_INTEGER
			je "sign"_zero_loop_int
			cmp rax, T_FRACTION
			je "sign"_zero_loop_frac
			jmp "sign"_zero_loop
			
			"sign"_zero_loop_int:
			mov rax, rbx
			DATA rax
			cmp rax, 0
			je "sign"_has_zero_param
			jmp "sign"_zero_loop

			"sign"_zero_loop_frac:
			mov rax, rbx
			CAR rax
			cmp rax, 0
			je "sign"_has_zero_param
			jmp "sign"_zero_loop

			"sign"_has_zero_param:
			"action"

			"sign"_end_zero_loop:
			"
	)))

(define impl-arithmetic
	(lambda (sign no_param_impl single_param_impl multiple_params_integers_impl multiple_params_fractions_impl check_zero_loop)
		(string-append
		"
		mov rbx, [rbp+8*3] ; n
		cmp rbx, 1
		jg "sign"_multiple_params
		cmp rbx, 0
		je "sign"_no_params

		; single param
		mov rbx, [rbp+8*4] ; first param 	
		mov rbx, [rbx]
		TYPE rbx

		cmp qword rbx, T_INTEGER
		je "sign"_single_param
		cmp qword rbx, T_FRACTION
		je "sign"_single_param
		jmp exit_compiler

		"sign"_single_param:
		; ============ UNIQUE SINGLE PARAM IMPLEMENTATION
		" single_param_impl "
		; ============ END
		jmp end"sign"

		"sign"_no_params:

		; ============ UNIQUE NO PARAM IMPLEMENTATION
		" no_param_impl "
		; ============ END
		jmp end"sign"		

		"sign"_multiple_params: ; assuming all is integer or fraction

		"check_zero_loop"

		mov r10, [rbp+8*3] ; n
		dec r10
		mov rdi, -1

		"sign"_loop:
		inc rdi
		cmp rdi, r10 
		je end"sign"
		push r10

		mov rbx, [rbp+8*(rdi+4)] 
		mov rcx, [rbp+8*(rdi+5)]
		push rbx
		push rcx
		call set_params_to_fractions ; rbx = (possibly new [rbp+8*(rdi+4)]) , rcx = (possibly new [rbp+8*(rdi+5)])
		pop rax ; just balance stack
		pop rax ; just balance stack
		
		; set the params back on the stack
		mov [rbp+8*(rdi+4)], rcx ; rcx is first
		mov [rbp+8*(rdi+5)], rbx ; rbx is second

		; decide if "sign" integers or fractions
		mov rbx, [rbp+8*(rdi+4)] ; first param 	
		mov rcx, [rbp+8*(rdi+5)] ; second param
		mov rbx, [rbx]
		mov rcx, [rcx]
		TYPE rbx
		TYPE rcx
		cmp rbx, rcx
		jne "sign"_fractions ;; not checking if not fraction/integer
		cmp rbx, T_INTEGER
		je "sign"_integers
		cmp rbx, T_FRACTION
		je "sign"_fractions
		jmp exit_compiler


		"sign"_fractions:

		; ============ UNIQUE IMPLEMENTATION

		"multiple_params_fractions_impl"

		; ============ END

		"sign"before_simplify_fraction:
		push r10 ; denominator to simplify
		push r8 ; numerator to simplify
		call simplify_fraction  ; rax = address of new T_FRACTION
		pop rbx ; just balance stack
		pop rbx ; just balance stack

		"sign"_endloop:
		mov [rbp+8*(rdi+5)], rax 
		pop r10
		jmp "sign"_loop

		"sign"_integers:
		mov rbx, [rbp+8*(rdi+4)] ; first param 	
		mov rcx, [rbp+8*(rdi+5)] ; second param
		mov rbx, [rbx]
		mov rcx, [rcx]
		DATA rbx
		DATA rcx

		; ============ UNIQUE IMPLEMENTATION
		" multiple_params_integers_impl "
		; ============ END

		SAFE_MALLOC 8 ; rax = SAFE_MALLOC
		mov [rax], rbx
		mov [rbp+8*(rdi+5)], rax 
		
		pop r10
		jmp "sign"_loop

		end"sign":
		mov r10, rax
		mov rbx, [rax] ; T_INTEGER or T_FRACTION
		TYPE rbx
		cmp rbx, T_FRACTION
		jne finish_"sign"

		; it is a fraction
		mov rbx, [rax] ; T_FRACTION
		CDR rbx ; denominator
		DATA rbx
		cmp rbx, 1 ; if denom is 1, build integer
		je "sign"_build_int
		cmp rbx, 0
		jl "sign"_denom_is_neg
		jmp finish_"sign"

		"sign"_build_int:
		mov rbx, [rax] ; T_FRACTION
		CAR rbx ; numerator
		DATA rbx
		sal rbx, TYPE_BITS
		or rbx, T_INTEGER

		mov [rax], rbx ; rax = previous SAFE_MALLOC
		jmp finish_"sign"

		"sign"_denom_is_neg:
		; turn it positive		
		mov rbx, [rax] ; T_FRACTION
		CDR rbx ; denominator
		DATA rbx
		mov rcx, 0
		sub rcx, rbx
		sal rcx, TYPE_BITS
		or rcx, T_INTEGER
		SAFE_MALLOC 8 ; rax = SAFE_MALLOC
		mov [rax], rcx ; 
		mov rdx, rax ; rdx = positive denom address

		; switch numer sign
		mov rbx, [r10] ; T_FRACTION
		CAR rbx ; numerator
		DATA rbx
		mov rcx, 0
		sub rcx, rbx
		sal rcx, TYPE_BITS
		or rcx, T_INTEGER
		SAFE_MALLOC 8 ; rax = SAFE_MALLOC
		mov [rax], rcx
		mov rcx, rax ; rcx = switched numer address

		sub rcx, start_of_data
		sal rcx, 34
		sub rdx, start_of_data
		sal rdx, TYPE_BITS
		or rdx, T_FRACTION
		or rcx, rdx
		SAFE_MALLOC 8 ; rax = SAFE_MALLOC
		mov [rax], rcx

		mov rbx, [rax] ; T_FRACTION
		CDR rbx ; denominator
		DATA rbx
		cmp rbx, 1 ; if denom is 1, build integer
		je "sign"_build_int2
		jmp finish_"sign"

		"sign"_build_int2:
		mov rbx, [rax] ; T_FRACTION
		CAR rbx ; numerator
		DATA rbx
		sal rbx, TYPE_BITS
		or rbx, T_INTEGER

		mov [rax], rbx ; rax = previous SAFE_MALLOC


		finish_"sign":
	")
		))






(define scheme-functions
	'(
		(define list (lambda x x))
		(define number? (lambda (x) (rational? x)))

		(define zero? (lambda (x) (= x 0)))
		
		(define maplist
			(letrec ((map1 
						(lambda (f s)
							(if (null? s)
								'()
								(cons (f (car s))
									(map1 f (cdr s))))
						)))
				(lambda (f s)
					(if (null? (car s)) 
						'()
						(cons (apply f (map1 car s))
							(maplist f (map1 cdr s)))))
				))

		(define map
			(let ((maplist maplist))
				(lambda (f . s)
					(maplist f s))))

		(define append
			(lambda args
				(letrec ((binary-append 
						(lambda (x y)
							(if (null? x)
								y
								(cons (car x) (binary-append (cdr x) y))))))
				(cond ((null? args) args)
					((null? (cdr args)) (car args))
		(else (binary-append (car args) (apply append (cdr args))))))))

		
		
		
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
		(my-map (lambda (exp)
			(let ((function-name (symbol->string (cadadr exp))))
				(print-line ";start generating function \"" function-name "\"
					"function-name":
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
