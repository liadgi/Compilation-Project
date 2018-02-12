(load "sexpr-parser.scm")
(load "tag-parser.scm")
(load "semantic-analyzer.scm")
(load "constants.scm")
(load "functions.scm")
(load "label-gen.scm")

(define pipeline
	(lambda (s)
		((star <sexpr>) s
			(lambda (m r)
				(map (lambda (e)
					(annotate-tc
						(pe->lex-pe
							(box-set
								(remove-applic-lambda-nil
									(parse e))))))
					m))
			(lambda (f) 'fail))))

(define file->list
	(lambda (in-file)
		(let ((in-port (open-input-file in-file)))
			(letrec ((run
			(lambda ()
				(let ((ch (read-char in-port)))
					(if (eof-object? ch)
						(begin
							(close-input-port in-port)
							'())
						(cons ch (run)))))))
				(run)))))


(define print-tabbed-line
	(lambda params
		(display (string-append "\t" (fold-right string-append "" params) "\n") output-port)))

(define print-line
	(lambda params
		(display (string-append (fold-right string-append "" params) "\n") output-port)))

(define gen-prologue-assembly
	(lambda ()
		(print-line "%include \"scheme.s\"")
		(print-line "section .data")
		(print-line "start_of_data:")
		))

(define gen-epilogue-assembly
	(lambda ()
		(begin (gen-clean-fake-env)
			(print-tabbed-line "L_error_cannot_apply_non_clos: ")
			(print-tabbed-line "ret")
			(print-tabbed-line "
				
				exit_compiler:
				push rbx
				mov esi, 1
				mov rax, 0
				mov rdi, .error_msg
				call printf
				pop rbx

				mov rax, 1
				mov rbx, 0
				int 0x80

				section .data
				.error_msg:
					db \"error occured.\", 0
				"))
))


(define gen-assembly-helpers
	(lambda () 3
		(print-line
		"jmp end_assembly_helpers
		simplify_fraction:
		push rbp
		mov rbp, rsp

		mov rbx, [rbp+8*4] 		; the fraction
		mov rbx, [rbx]
		mov rcx, rbx ; take numerator
		CAR rcx
		mov rdx, rbx ; take denominator
		CAR rdx
		push rcx
		push rdx
		call gcd
		pop rdx
		pop rcx
		; rax = T_INTEGER result


		pop rbp
		ret


		fractions_greater_equal_or_less:
		push rbp
		mov rbp, rsp			
		

		mov rbx, [rbp+8*4] 		; the first
		mov rcx, [rbp+8*5] 		; the second

		mov rax, -1 ; left is less
		mov rax, 0 ; equal
		mov rax, 1 ; left is greater
		pop rbp
		ret


		convert_integer_to_fraction: ; result pointer (fraction) in rax
		push rbp
		mov rbp, rsp

	
		; we want a fraction where the numerator is the integer,
		; and the denominator is 1.

		; set the 1 as the denominator:
		mov rbx, 1
		shl rbx, TYPE_BITS
		or rbx, T_INTEGER
		SAFE_MALLOC 8 ; rax = SAFE_MALLOC
		mov [rax], rbx
		; rax = pointer to T_INTEGER 1
		mov rcx, rax

		mov rbx, [rbp+8*2] 		; the integer address
		; copy
		mov rbx, [rbx]
		SAFE_MALLOC 8 ; rax = SAFE_MALLOC
		mov [rax], rbx
		mov rbx, rax

		sub rbx, start_of_data
		mov rdx, rbx
		shl rdx, 34
		mov rbx, rcx
		sub rbx, start_of_data
		shl rbx, TYPE_BITS ;;;; CHECK THIS ONE!
		or rdx, rbx
		or rdx, T_FRACTION
		; rdx = sobIntX sobInt1 T_FRACTION

		SAFE_MALLOC 8 ; rax = SAFE_MALLOC
		mov [rax], rdx

		pop rbp
		ret ; result pointer (fraction) in rax


		set_params_to_fractions:
		push rbp
		mov rbp, rsp

		mov rbx, [rbp+8*2] 		; the first
		mov rcx, [rbp+8*3] 		; the second
		mov rbx, [rbx]
		mov rcx, [rcx]

		TYPE rbx
		TYPE rcx
		cmp rbx, T_FRACTION
		je first_param_is_fraction
		cmp rcx, T_FRACTION
		je first_int_2nd_fract
		jmp set_params_to_fractions_dont_touch ; both params are integers

		first_param_is_fraction:
		cmp rcx, T_INTEGER
		je first_fract_2nd_int
		jmp set_params_to_fractions_dont_touch ; both params are fractions

		first_int_2nd_fract:
		mov rax, [rbp+8*2]
		push rax
		call convert_integer_to_fraction ; rax = 1st param as fraction, address
		pop r11
		mov rbx, rax
		mov rcx, [rbp+8*3] 		; the second
		jmp end_set_params_to_fractions

		first_fract_2nd_int:
		mov rax, [rbp+8*3]
		push rax
		call convert_integer_to_fraction ; rax = 2nd param as fraction, address
		pop r11
		mov rbx, [rbp+8*2] 		; the first
		mov rcx, rax
		jmp end_set_params_to_fractions

		set_params_to_fractions_dont_touch:
		mov rbx, [rbp+8*2] 		; the first
		mov rcx, [rbp+8*3] 		; the second

		jmp end_set_params_to_fractions


		end_set_params_to_fractions:

		pop rbp
		ret

		gcd: ; num, denom
		push rbp
		mov rbp, rsp

		;start
		mov rax, [rbp+8*4] 		; the numerator
		mov rax, [rax]
		shr rax, TYPE_BITS
		mov rbx, [rbp+8*5] 		; the denominator
		mov rbx, [rbx]
		shr rbx, TYPE_BITS

		gcd_loop:
		cmp rbx, 0
		je end_gcd_loop
		mov r10, rbx ; t = b

		; b = a mod b
		mov rdx, 0
		div rbx ; rdx = a mod b
		mov rbx, rdx ; b = a mod b

		mov rax, r10 ; a = t

		jmp gcd_loop
		end_gcd_loop:

		; res in rax
		shl rax, TYPE_BITS
		or rax, T_INTEGER

		mov rbx, rax
		SAFE_MALLOC 8 ; rax = SAFE_MALLOC
		mov [rax], rbx
		
		;end

		pop rbp
		ret


		end_assembly_helpers:
		")
		))


;write_sob_undefined:
;	push rbp
;	mov rbp, rsp
;
;	mov rax, 0
;	mov rdi, .undefined
;	call printf
;
;	leave
;	ret

(define gen-make-literal-nil
	(lambda ()
		(print-line "sobNil:")
		(print-tabbed-line "dq SOB_NIL")
		))

(define gen-make-literal-true
	(lambda ()
		(print-line "sobTrue:")
		(print-tabbed-line "dq SOB_TRUE")
		))

(define gen-make-literal-false
	(lambda ()
		(print-line "sobFalse:")
		(print-tabbed-line "dq SOB_FALSE")
		))

(define gen-make-literal-void
	(lambda ()
		(print-line "sobVoid:")
		(print-tabbed-line "dq SOB_VOID")
		))

(define gen-make-literal-integer
	(lambda (value label)
		(let ((strVal (number->string value)))
			(print-line label ":")
			(print-tabbed-line "dq MAKE_LITERAL(T_INTEGER, " strVal ")")
		)))

(define gen-make-literal-char
	(lambda (value label)
		(let ((c (char->integer value)))
			(print-line label ":")
			(print-tabbed-line "dq MAKE_LITERAL(T_CHAR, " (number->string c) ")")
		)))

(define gen-make-literal-pair
	(lambda (label rest)
		(let ((first-label (car rest))
			  (second-label (cadr rest)))
			(print-line label ":")
			(print-tabbed-line "dq MAKE_LITERAL_PAIR(" first-label ", " second-label ")")
		)
		))

(define gen-make-literal-vector
	(lambda (label rest)
			(let ((str (gen-vector-refs rest "")))
					(print-line label ":")
					(print-tabbed-line "MAKE_LITERAL_VECTOR " str )
				)
		))

(define gen-string
	(lambda (str)
		(let* ((lst (string->list str))
			(elem (car lst))
			(res 
				(cond ((eq? elem #\space) "CHAR_SPACE, \"" )
						((eq? elem #\tab) "CHAR_TAB, \"")
						(else (string-append "\"" (string elem))))))
		(letrec ((run (lambda (lst res)
			(if (null? (cdr lst))
				(let ((elem (car lst))) 
					(cond ((eq? elem #\space) (string-append res "\" , CHAR_SPACE"))
						  ((eq? elem #\tab) (string-append res "\" , CHAR_TAB"))
						  (else (string-append res (string elem) "\"")))
				)
				(let ((elem (car lst))) 
					(cond ((eq? elem #\space) (run (cdr lst) (string-append res (string #\") ", CHAR_SPACE, " (string #\"))))
						  ((eq? elem #\tab) (run (cdr lst) (string-append res ", CHAR_TAB, \"")))
						  (else (run (cdr lst) (string-append res (string elem))))
				))
			
			))))
			(run (cdr lst) res)
		))
))


(define gen-make-literal-string
	(lambda (value label)
					(print-line label ":")
					(print-tabbed-line "MAKE_LITERAL_STRING \"" value "\"")
		))


(define gen-vector-refs
	(lambda (rest str)
			(if (null? (cdr rest)) (string-append str (car rest))
			 (gen-vector-refs (cdr rest) (string-append str (car rest) ", ")))
		))

(define gen-make-literal-fraction
	(lambda (label rest)
		(let ((first (car rest))
			  (second (cadr rest)))
			(print-line label ":")
			(print-tabbed-line "dq MAKE_LITERAL_FRACTION(" first ", " second ")")
		)))



(define gen-constants-assembly
	(lambda (table)
		(print-line ";start gen-constants-assembly")
		(letrec ((gen (lambda (table)
			(if (not (null? table))
				(begin
					;(display-newline (car table))
					(let* ((first-pair (car table))
						   (value (car first-pair))
						   (address (cadr first-pair))
						   (type (caddr first-pair))
						   (label (cadddr first-pair))
						   (rest (cddddr first-pair))
						   )
						(cond ((eq? type 'T_NIL) (gen-make-literal-nil))
							  ((eq? type 'T_BOOL) (if value (gen-make-literal-true) (gen-make-literal-false)))
							  ((eq? type 'T_VOID) (gen-make-literal-void))
							  ((eq? type 'T_INTEGER) (gen-make-literal-integer value label))
							  ((eq? type 'T_CHAR) (gen-make-literal-char value label))
							  ((eq? type 'T_PAIR) (gen-make-literal-pair label rest))
							  ((eq? type 'T_VECTOR) (gen-make-literal-vector label rest))
							  ((eq? type 'T_FRACTION) (gen-make-literal-fraction label rest))
							  ((eq? type 'T_STRING) (gen-make-literal-string value label))
							  (else (display-error "gen-constants-assembly: type " type " does not exist")))

						)
					(gen (cdr table))
					)

				)
				)))
		(gen table))
		(print-line ";end gen-constants-assembly
			")
		
		)
)

#;(define symbol-gen
	(lambda (...)

))


(define gen-section-bss
	(lambda ()
		(print-line "")
		(print-line "section .bss")
		(print-line "global main")
))

(define gen-fake-env
	(lambda ()
			(print-tabbed-line "; starting fake env generation")
			(print-tabbed-line "mov rax, 0")
			(print-tabbed-line "push rax") ;fake n
			(print-tabbed-line "push SOB_NIL") ;fake env
			(print-tabbed-line "push rax") ;fake ret
			(print-tabbed-line "push rbp")
			(print-tabbed-line "mov rbp, rsp")
			(print-tabbed-line "; ending fake env generation")
		))

(define gen-clean-fake-env
	(lambda ()
		(print-line "")
		(print-line "leave")
		(print-line "pop rax") ;pop ret
		(print-line "pop rbx") ;pop env
		(print-line "pop rax") ;pop n

		))

(define gen-section-text
	(lambda ()
		(begin
			(print-line "")
			(print-line "section .text")
			(print-line "main:
				mov rax, malloc_pointer
				mov qword [rax], start_of_memory")
			)
))

(define write_sob
	(lambda ()
			(print-tabbed-line "push qword[rax]")
			(print-tabbed-line "call write_sob_if_not_void")
			(print-tabbed-line "add rsp, 1*8")
))

(define const-gen
	(lambda (value const-table)
		(let* ((const-label (lookup-constant-get-label value const-table)))
			(print-tabbed-line "mov rax, " const-label )
			)
))

(define seq-gen ; e1,e2,...,en
	(lambda (elems const-table major fvars-table)
		(if (null? (cdr elems))
					(code-gen (car elems) const-table major fvars-table) ; return en
					(begin
						(code-gen (car elems) const-table major fvars-table)
						(seq-gen (cdr elems) const-table major fvars-table)
						)
					)
))

(define or-gen
	(lambda (exprs jmpLabel constants-table major fvars-table)
		(if (null? (cdr exprs)) 
				(begin
					(code-gen (car exprs) constants-table major fvars-table)
					(print-tabbed-line jmpLabel ":"))
				(begin 
					(code-gen (car exprs) constants-table major fvars-table)
					(print-tabbed-line "mov rbx, [rax]")
					(print-tabbed-line "cmp rbx, SOB_FALSE")
					(print-tabbed-line "jne " jmpLabel)
					(or-gen (cdr exprs) jmpLabel constants-table major fvars-table))
			)
		))

(define if-gen
	(lambda (exprs ifLabel ifendLabel constants-table major fvars-table)
		(let ((test (car exprs))
				(dit (cadr exprs))
				(dif (caddr exprs)))
			(begin 
				(code-gen test constants-table major fvars-table)
				(print-tabbed-line "mov rax, qword[rax]")
				(print-tabbed-line "cmp rax, SOB_FALSE")
				(print-tabbed-line "je " ifLabel)
				(code-gen dit constants-table major fvars-table)
				(print-tabbed-line "jmp " ifendLabel)
				(print-tabbed-line ifLabel ":")
				(code-gen dif constants-table major fvars-table)
				(print-tabbed-line ifendLabel ":"))
		)))


(define lambda-simple-gen
	(lambda (exprs constants-table major fvars-table)
		(let ((params (car exprs))
				(body (cadr exprs))
				(labeloop (loop-label))
				(labelEndLoop (loop-end-label))
				(labeloop2 (loop-label))
				(labelEndLoop2 (loop-end-label))
				(labelLambda (lambda-label))
				(labelEndLambda (lambda-end-label)))
			(print-line "
					;lambda-simple generation
					;adding new-line to the new extended env
					SAFE_MALLOC " (number->string (* 8 (+ 1 major)))

					
					"; rax = SAFE_MALLOC
					mov rbx, rax 			;rbx <-- malloc(8*(major+1))
					;for loop - copy previous env to the new extended env
					mov rax, " (number->string major) ";rax <-- major
					mov rdi, 0
					mov rcx, [rbp+8*2] ;rcx <-- env
					"
					labeloop":
					cmp rdi, rax
					je " labelEndLoop"
						;rbx[i+1] = env[i]
					mov rdx, [rcx+8*rdi] ; 
					inc rdi
					mov [rbx+8*rdi], rdx
					jmp " labeloop "
					"labelEndLoop":

					;creating env -> the new extended env
					;if this is the fake env, 
					mov rdx, [rbp+8*3] 		;rdx <-- n
					shl rdx, 3 				;rdx <-- n*8
					SAFE_MALLOC rdx
					
					mov rcx, rax 			;rcx <-- malloc(8*n)
					mov rax, [rbp+8*3] 		;rax <-- n
					mov rdi, 0
					;if this starts from the fake env, the loop is skipped
					"
					labeloop2":
					cmp rdi, rax
					je " labelEndLoop2"
						;rcx[i] = param[i]
					mov rdx, [rbp+8*(rdi+4)] 	;rdx <-- param[i]
					mov[rcx+8*rdi], rdx 	;rcx[i] <-- param[i]
					inc rdi
					jmp " labeloop2 "
					"labelEndLoop2":

					mov [rbx], rcx
					
					SAFE_MALLOC 16
					
					; rax = address from SAFE_MALLOC. rbx = env
					MAKE_LITERAL_CLOSURE rax, rbx, " labelLambda"
					;mov rax, [rax]

					jmp " labelEndLambda"
					"labelLambda":
					push rbp
					mov rbp, rsp

					;generating lambda body")
			(code-gen body constants-table (+ 1 major) fvars-table)
			(print-line "
					mov rbx, [rbp+8*3] 		;rbx <-- n
					leave
					pop rcx					;rcx <-- ret
					add rbx, 2
					shl rbx, 3
					add rsp, rbx 			;clean stack
					push rcx
					ret

					;pop rbp
					;ret 
					"labelEndLambda":
				")
		)
	))

(define my-map
	(lambda (proc lst)
		(if (null? lst) lst
			(cons (proc (car lst)) (my-map proc (cdr lst))))
		))

(define applic-gen
	(lambda (applic-body constants-table major fvars-table)
		(let* 	((proc (car applic-body))
				(params (reverse (cadr applic-body)))
				(n (length params)))
			(print-line "
				;start applic-gen

				;push SOB_NIL
				;pushing arguments")
			(my-map (lambda (param)
				(code-gen param constants-table major fvars-table) 
				(print-line "push rax"))
				params)
			(print-line "
				push " (number->string n) " ;number of params" )
			(code-gen proc constants-table major fvars-table)
			(print-line "
				;push env
				mov rax, [rax]
				mov rbx, rax 	;rbx <-- closure(?)
				TYPE rbx
				cmp rbx, T_CLOSURE
				jne L_error_cannot_apply_non_clos
				mov rbx, rax
				CLOSURE_ENV rbx
				push rbx
				CLOSURE_CODE rax
				call rax
				;add rsp,  (number->string (* (+ 3 n) 8))

				;end applic-gen
				"
				))
	))

(define pvar-gen
	(lambda (pvar-body)
		(let ((minor (cadr pvar-body)))
			(print-line "
				;pvar-gen
				mov rax, [rbp +" (number->string (* 8 (+ 4 minor))) "]
			")
		)
	))

(define set-pvar-gen
	(lambda (set-pvar-body constants-table major fvars-table)
		(let ((minor (caddar set-pvar-body))
				(value (cadr set-pvar-body)))
			(code-gen value constants-table major fvars-table)
			(print-line "
				;set-pvar-gen
				mov qword[rbp +" (number->string (* 8 (+ 4 minor))) "], rax
				mov rax, sobVoid
			")
		)
	))

(define bvar-gen
	(lambda (bvar-body)
		(let ((major (cadr bvar-body))
				(minor (caddr bvar-body)))
			(print-line "
				;bvar-gen
				mov rax, qword[rbp +" (number->string (* 8 2)) "] 	;env
				mov rax, qword[rax +" (number->string (* 8 major)) "] 	;env[major]
				mov rax, qword[rax +" (number->string (* 8 minor)) "]	;env[major][minor]
			")
		)
	))

(define set-bvar-gen
	(lambda (set-bvar-body constants-table major fvars-table)
		(let* ((major-minor (cddar set-bvar-body))
				(major (car major-minor))
				(minor (cadr major-minor))
				(value (cadr set-bvar-body)))
			(code-gen value constants-table major fvars-table)
			(print-line "
				;set-bvar-gen
				mov rbx, qword[rbp+2*8]
				mov rbx, qword[rbx +" (number->string (* 8 major)) "]
				mov qword[rbx +" (number->string (* 8 minor)) "], rax
				mov rax, sobVoid
			")
		)
	))

(define fvar-gen
	(lambda (fvar fvars)
		(let ((label (lookup-fvar-get-label fvar fvars)))
			(print-tabbed-line ";fvar-gen
				mov rax, [" label "] ;fvar-gen")
			)
))

(define set-fvar-gen
	(lambda (set-fvar-body constants-table major fvars-table)
		(let* ((fvar-name (cadar set-fvar-body))
			   (value (cadr set-fvar-body))
			   (label (lookup-fvar-get-label fvar-name fvars-table)))
			(code-gen value constants-table major fvars-table)
			(print-tabbed-line ";set-fvar-gen
				mov [" label "], rax
				mov rax, sobVoid" )
		)
	))


(define lambda-opt-gen
	(lambda (exprs constants-table major fvars-table)
		(let ((params (car exprs))
				(body (caddr exprs))
				(labeloop (loop-label))
				(labelEndLoop (loop-end-label))
				(labeloop2 (loop-label))
				(labelEndLoop2 (loop-end-label))
				(labelLambda (lambda-label))
				(labelEndLambda (lambda-end-label))
				(labelLessArgumentsThanParams (lambda-less-args-than-params-label))
				(labelLoopFixStack (loop-fix-stack-label))
				(labelLoopFixStackEnd (loop-fix-stack-end-label))
				(labelLoopFixStack2 (loop-fix-stack-label))
				(labelLoopFixStackEnd2 (loop-fix-stack-end-label))
				(labelLoopFixStack3 (loop-fix-stack-label))
				(labelLoopFixStackEnd3 (loop-fix-stack-end-label))
				(endOptionalParams (loop-fix-stack-end-label))
				)
			(print-line "
					;lambda-opt generation
					;adding new-line to the new extended env
					SAFE_MALLOC " (number->string (* 8 (+ 1 major)))
					"
					mov rbx, rax 			;rbx <-- malloc(8*(major+1))
					;for loop - copy previous env to the new extended env
					mov rax, " (number->string major) ";rax <-- major
					mov rdi, 0
					mov rcx, [rbp+8*2] ;rcx <-- env
					"
					labeloop":
					cmp rdi, rax
					je " labelEndLoop"
						;rbx[i+1] = env[i]
					mov rdx, [rcx+8*rdi]
					inc rdi
					mov [rbx+8*rdi], rdx
					jmp " labeloop "
					"labelEndLoop":

					;creating env -> the new extended env
					mov rdx, [rbp+8*3] 		;rdx <-- n
					shl rdx, 3 				;rdx <-- n*8
					SAFE_MALLOC rdx
					mov rcx, rax 			;rcx <-- malloc(8*n)
					mov rax, [rbp+8*3] 		;rax <-- n
					mov rdi, 0
					"
					labeloop2":
					cmp rdi, rax
					je " labelEndLoop2"
						;rcx[i] = param[i]
					mov rdx, [rbp+8*(rdi+4)] 	;rdx <-- param[i]
					mov[rcx+8*rdi], rdx 	;rcx[i] <-- param[i]
					inc rdi
					jmp " labeloop2 "
					"labelEndLoop2":

					mov [rbx], rcx
					SAFE_MALLOC 16
					MAKE_LITERAL_CLOSURE rax, rbx, " labelLambda"
					jmp " labelEndLambda"
					"labelLambda":
					push rbp
					mov rbp, rsp

					"
					; ((lambda (x y . z) ... ) 1 2)
					; ((lambda z ... ) )

					; ((lambda (x y . z) ... ) 1 2 3)
					; ((lambda (x y . z) ... ) 1 2 3 4 5)
					; ((lambda z ... ) 1 2 3)
					"

					;FIXING STACK
					mov r8, sobNil
					mov rdi, [rbp+8*3]					;rdi <-- n
					mov r9, "(number->string (+ (length params) 1))" 	;t9 <-- |params|"

					"
					cmp rdi, r9
					jl "labelLessArgumentsThanParams"
					"			
 					labelLoopFixStack": ; create a linked list of pairs, and put them instead of the first opt parameter
 					cmp rdi, r9
 					jl "labelLoopFixStackEnd "
 					mov r10, [rbp+8*(rdi+3)] 			;param[rdi]
 					"(debug-label)":
 					MAKE_PAIR r10, r8 ; r10 now holds the pair
 					SAFE_MALLOC 8 ; rax holds the allocated space
 					mov [rax], r10 ; put the pair in allocated memory
 					mov r8, rax ; r8 now holds the address of the pair
 					dec rdi
 					jmp "labelLoopFixStack "
 					"
 					labelLoopFixStackEnd":

 					;mov [rbp+8*(r9+3)], r8 ; replace the first opt parameter with the new linked list.
 					;the 3 is for skipping ret, env, n.

 					mov rdi, [rbp+8*3]					;rdi <-- n
 					lea rbx, [rbp+8*(rdi+3)] ; the place of the last parameter (n)
 					mov [rbx], r8 ; put the linked list / NIL as the last parameter of the fixed function
 					; put the real parameters above this list, in the stack
 					lea r10, [rbp+8*(r9+2)] ; get to the last real parameter
 					mov rsp, rbx

 					; the formula: 
 					mov r11, 3
 					add r11, r9 ; r11 = # of values to pop and override

 					"
 					labelLoopFixStack2":
 					cmp r11, 0 ; 
 					je "labelLoopFixStackEnd2 "
 					push qword [r10]
 					sub r10, 8
 					dec r11
 					jmp "labelLoopFixStack2
 					"

 					"labelLoopFixStackEnd2 ": ; done overriding
 					mov rbp, rsp
 					mov [rbp+8*3], r9					; n becomes |params|

 					jmp "endOptionalParams"




 					"labelLessArgumentsThanParams":

 					;inc qword [rbp+8*3]

 					;mov [rbp+8*(r9+3)], r8 ; replace the first opt parameter with the new linked list.


 					mov rdi, [rbp+8*3]					;rdi <-- n
 					lea rbx, [rbp+8*(rdi+3)] ; the last cell in the frame (last parameter or n)

 					;mov [rbx], r8 ; put the NIL as the last parameter of the fixed function
 					; put the real parameters above this list, in the stack
 					lea r10, [rbp+8*(r9+2)] ; get to the last real parameter
 					mov r12, [r10] ; get the value of the last real parameter
 					mov rsp, rbx

 					; the formula: 
 					mov r11, 3
 					add r11, r9 ; r11 = # of values to pop and override

 					mov r13, r8 ; NIL
 					mov [r10], r13
 					"
 					labelLoopFixStack3":
 					cmp r11, 0 ; 
 					je "labelLoopFixStackEnd3 "
 					sub r10, 8
 					mov r13, [r10]
 					push r12
 					mov r12, r13
 					dec r11
 					jmp "labelLoopFixStack3
 					"
 					push r12 ; rbp
 					"labelLoopFixStackEnd3 ": ; done overriding
 					mov rbp, rsp
 					;mov [rbp+8*3], r9					; n becomes |params|
 					inc qword [rbp+8*3]



 					"endOptionalParams ":

					; start generating lambda body")

			(code-gen body constants-table (+ 1 major) fvars-table)
			(print-line "
				   ; end generating lambda body

					mov rbx, [rbp+8*3] 		;rbx <-- n
					leave
					pop rcx					;rcx <-- ret
					add rbx, 2
					shl rbx, 3
					add rsp, rbx 			;clean stack
					push rcx
					ret 

					;pop rbp
					;ret					
					"labelEndLambda":
				")
		)
	))

(define box-gen
	(lambda (box-body constants-table major fvars-table)
		(code-gen box-body constants-table major fvars-table)
		(print-line "
			mov rbx, rax
			SAFE_MALLOC 8
			mov qword[rax], rbx
			")
))

(define box-get-gen
	(lambda (box-body constants-table major fvars-table)
		(code-gen box-body constants-table major fvars-table)
		(print-line  "mov rax, [rax]	;unbox")
))

(define box-set-gen
	(lambda (box-set-body constants-table major fvars-table)
		(let ((var (car box-set-body))
				(value (cadr box-set-body)))
		(code-gen value constants-table major fvars-table)
		(print-line  "mov rbx, rax")
		(code-gen var constants-table major fvars-table)
		(print-line  "mov [rax], rbx")
		(print-line  "mov rax, sobVoid")
)))

(define tc-applic-gen
	(lambda (tc-applic-body constants-table major fvars-table)
		(let* 	((proc (car tc-applic-body))
				(params (reverse (cadr tc-applic-body)))
				(m (length params))
				(tc-applic-loop (tc-loop-label))
				(tc-applic-loop-end (tc-loop-end-label)))
			(print-line "
				;start tc-applic-gen
				;pushing arguments")
			(my-map (lambda (param)
				(code-gen param constants-table major fvars-table) 
				(print-line "push rax"))
				params)
			(print-line "
				;push m
				push " (number->string m))
			(code-gen proc constants-table major fvars-table)
			(print-line "
				;push env
				mov rax, [rax]
				mov rbx, rax 	;rbx <-- closure(?)
				TYPE rbx
				cmp rbx, T_CLOSURE
				jne L_error_cannot_apply_non_clos
				mov rbx, rax
				CLOSURE_ENV rbx
				push rbx
				CLOSURE_CODE rax

				; TC-APPLIC
				push qword[rbp+8]		;push old ret
				mov r8, rbp 			;save current rbp
				mov rbp, [rbp] 			;rbp <-- previous rbp
				mov r11, [r8+3*8]		;r11 <-- previous n = n
				mov r9, r11
				add r9, 3
				shl r9, 3				;r9 <-- (n+3)*8
				add r9, r8				;r9 <-- r8 + (n+3)*8
				mov r10, r9				;previous last param 
				sub r8, 8				;current last param
				;loop - replacing stack

				mov rdi, 0
				" tc-applic-loop":
				cmp rdi, " (number->string (+ m 4))"
				je " tc-applic-loop-end"
				mov r9, [r8]
				mov [r10], r9
				sub r8, 8
				sub r10, 8
				inc rdi
				jmp "tc-applic-loop"
				"tc-applic-loop-end":

				add r11, 4				;r11 <-- 4+n
				shl r11, 3				;r11 <-- 8*(4+n)
				add rsp, r11
				jmp rax

				;end tc-applic-gen"
				))

	))

(define code-gen
	(lambda (ast constants-table major fvars-table)
		(cond ((eq? (car ast) 'const) (const-gen (cadr ast) constants-table)) 
			  ((eq? (car ast) 'seq) (seq-gen (cadr ast) constants-table major fvars-table))
			  ((eq? (car ast) 'or) (or-gen (cadr ast) (or-label) constants-table major fvars-table))
			  ((eq? (car ast) 'if3) (if-gen (cdr ast) (if-label) (if-end-label) constants-table major fvars-table))
			  ((eq? (car ast) 'lambda-simple) (lambda-simple-gen (cdr ast) constants-table major fvars-table))
			  ((eq? (car ast) 'applic) (applic-gen (cdr ast) constants-table major fvars-table))
			  ((eq? (car ast) 'pvar) (pvar-gen (cdr ast)))
			  ((and (eq? (car ast) 'set) (eq? (caadr ast) 'pvar)) (set-pvar-gen (cdr ast) constants-table major fvars-table))
			  ((eq? (car ast) 'bvar) (bvar-gen (cdr ast)))
			  ((and (eq? (car ast) 'set) (eq? (caadr ast) 'bvar)) (set-bvar-gen (cdr ast) constants-table major fvars-table))
			  ((eq? (car ast) 'fvar) (fvar-gen (cadr ast) fvars-table))
			  ((and (or (eq? (car ast) 'set) (eq? (car ast) 'define)) (eq? (caadr ast) 'fvar)) (set-fvar-gen (cdr ast) constants-table major fvars-table))
			  ((eq? (car ast) 'lambda-opt) (lambda-opt-gen (cdr ast) constants-table major fvars-table))
			  ((eq? (car ast) 'box) (box-gen (cadr ast) constants-table major fvars-table))
			  ((eq? (car ast) 'box-get) (box-get-gen (cadr ast) constants-table major fvars-table))
			  ((eq? (car ast) 'box-set) (box-set-gen (cdr ast) constants-table major fvars-table))
			  ((eq? (car ast) 'tc-applic) (tc-applic-gen (cdr ast) constants-table major fvars-table))
		)
))

(define gen-fvars-assembly
	(lambda (fvars)
			(print-line  ";start gen-fvars-assembly")
			(map (lambda (fvar) 
				(print-line (cadr fvar) ":
					dq SOB_UNDEFINED"))
				fvars)
			(print-line  ";end gen-fvars-assembly
				")
	))



(define gen-code-assembly
	(lambda (asts constants-table fvars)
		(if (null? asts) (void)
			(begin
				(begin (code-gen (car asts) constants-table 0 fvars)
				 	   (if
				 	   (not 
				 	   	(or 
				 	   		(eq? (caar asts) 'define) 
				 	   		(eq? (caar asts) 'set)))
				 	   	(write_sob)))
				(gen-code-assembly (cdr asts) constants-table fvars)
			)
		)
		))


(define frame-gen
	(lambda (structure)
		(let* ((constants-table (car structure))
				(fvars (cadr structure))

				(prologue-assembly (gen-prologue-assembly)) ; include scheme.s, section data, start of data
				(constants-assembly (gen-constants-assembly constants-table)) ; sobNil: dq SOB_NIL, sobVoid...
				(fvars-assembly (gen-fvars-assembly fvars)) ; Lglob_1: dq SOB_UNDEFINED, Lglob_2...
				(section-bss (gen-section-bss))
				(section-text (gen-section-text)) ; malloc_pointer
				(assembly-helpers (gen-assembly-helpers))
				(funcs-in-assembly (init-functions fvars)) ; cons, ...
				(fake-env (gen-fake-env))
				(funcs-in-scheme (gen-global-functions scheme-expressions constants-table fvars)) ; list, ...

				(code-assembly (gen-code-assembly (caddr structure) constants-table fvars))
				(epilogue-assembly (gen-epilogue-assembly))
				)
			constants-assembly
			)
		)
)

(define output-port #f)

(define compile-scheme-file
	(lambda (file output-file)
		(let* ((asts (pipeline (file->list file)))
			   (const-table (build-constants-table (append asts scheme-expressions)))
			   (fvars-table (label-fvars '() (get-fvars-list asts)))
			   )
		;asts
		;fvars-table
		;const-table
		(begin 
			(delete-file output-file) 
			(set! output-port (open-output-file output-file))
			(frame-gen (list const-table fvars-table asts))
			;(code-gen (list const-table symbol-table freevars ast))
			(close-output-port output-port))
				))
)

(define list->set
	(lambda (s)
		(fold-right 
			(lambda (a s)
				(if (ormap (lambda (si) (equal? a si)) s)
					s 
					(cons a s)))
			'()
			s)
))


(define write-to-target-file
	(lambda (assembly-code output-file)
			(write assembly-code output-port)
			(close-output-port output-port)
		)
)

