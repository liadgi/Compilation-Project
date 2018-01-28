(load "sexpr-parser.scm")
(load "tag-parser.scm")
(load "semantic-analyzer.scm")
(load "constants.scm")
(load "fvars.scm")
(load "functions.scm")

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


(define concat-strings
	(lambda args 
		(if (= (length args) 1) 
			(car args)
			(string-append (car args) (apply concat-strings (cdr args))))
		))


(define print-tabbed-line
	(lambda (line)
		(display (concat-strings "\t" line "\n") output-port)
		))

(define print-line
	(lambda (line)
		(display (concat-strings line "\n") output-port)
		))

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
			(print-tabbed-line "ret"))
))

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
			(print-line (concat-strings label ":"))
			(print-tabbed-line (concat-strings "dq MAKE_LITERAL(T_INTEGER, " strVal ")"))
		)))

(define gen-make-literal-char
	(lambda (value label)
		(let ((c (char->integer value)))
			(print-line (concat-strings label ":"))
			(print-tabbed-line (concat-strings "dq MAKE_LITERAL(T_CHAR, " (number->string c) ")"))
		)))

(define gen-make-literal-pair
	(lambda (label rest)
		(let ((first-label (car rest))
			  (second-label (cadr rest)))
			(print-line (concat-strings label ":"))
			(print-tabbed-line (concat-strings "dq MAKE_LITERAL_PAIR(" first-label ", " second-label ")"))
		)
		))

(define gen-make-literal-vector
	(lambda (label rest)
			(let ((str (gen-vector-refs rest "")))
					(print-line (concat-strings label ":"))
					(print-tabbed-line (concat-strings "MAKE_LITERAL_VECTOR " str ))
				)
		))

(define gen-string
	(lambda (str)
		(let* ((lst (string->list str))
			(elem (car lst))
			(res 
				(cond ((eq? elem #\space) "CHAR_SPACE, \"" )
						((eq? elem #\tab) "CHAR_TAB, \"")
						(else (concat-strings "\"" (string elem))))))
		(letrec ((run (lambda (lst res)
			(if (null? (cdr lst))
				(let ((elem (car lst))) 
					(cond ((eq? elem #\space) (string-append res "\" , CHAR_SPACE"))
						  ((eq? elem #\tab) (concat-strings res "\" , CHAR_TAB"))
						  (else (concat-strings res (string elem) "\"")))
				)
				(let ((elem (car lst))) 
					(cond ((eq? elem #\space) (run (cdr lst) (string-append res (string #\") ", CHAR_SPACE, " (string #\"))))
						  ((eq? elem #\tab) (run (cdr lst) (concat-strings res ", CHAR_TAB, \"")))
						  (else (run (cdr lst) (concat-strings res (string elem))))
				))
			
			))))
			(run (cdr lst) res)
		))
))


(define gen-make-literal-string
	(lambda (value label)
					(print-line (concat-strings label ":"))
					(print-tabbed-line (concat-strings "MAKE_LITERAL_STRING \"" value "\""))
		))


(define gen-vector-refs
	(lambda (rest str)
			(if (null? (cdr rest)) (concat-strings str (car rest))
			 (gen-vector-refs (cdr rest) (concat-strings str (car rest) ", ")))
		))

(define gen-make-literal-fraction
	(lambda (label rest)
		(let ((first (car rest))
			  (second (cadr rest)))
			(print-line (concat-strings label ":"))
			(print-tabbed-line (concat-strings "dq MAKE_LITERAL_FRACTION(" first ", " second ")"))
		)))



(define gen-constants-assembly
	(lambda (table)
		(letrec ((gen (lambda (table)
			(if (not (null? table))
				(begin 
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
							  ((eq? type 'T_INT) (gen-make-literal-integer value label))
							  ((eq? type 'T_CHAR) (gen-make-literal-char value label))
							  ((eq? type 'T_PAIR) (gen-make-literal-pair label rest))
							  ((eq? type 'T_VECTOR) (gen-make-literal-vector label rest))
							  ((eq? type 'T_FRACTION) (gen-make-literal-fraction label rest))
							  ((eq? type 'T_STRING) (gen-make-literal-string value label))
							  (else "DO_LATER "))

						)
					(gen (cdr table))
					)

				)
				)))
		(gen table))
		
		
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
			;(print-tabbed-line (concat-strings "mov rax, [rax]"))
			(print-tabbed-line (concat-strings "push qword[rax]"))
			(print-tabbed-line "call write_sob_if_not_void")
			(print-tabbed-line "add rsp, 1*8")
))

(define const-gen
	(lambda (value const-table)
		(let* ((const-label (lookup-constant-get-label value const-table)))
			(print-tabbed-line (concat-strings "mov rax, " const-label ))
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
					(print-tabbed-line (concat-strings jmpLabel ":")))
				(begin 
					(code-gen (car exprs) constants-table major fvars-table)
					(print-tabbed-line "mov rbx, [rax]")
					(print-tabbed-line "cmp rbx, SOB_FALSE")
					(print-tabbed-line (concat-strings "jne " jmpLabel))
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
				(print-tabbed-line (concat-strings "je " ifLabel))
				(code-gen dit constants-table major fvars-table)
				(print-tabbed-line (concat-strings "jmp " ifendLabel))
				(print-tabbed-line (concat-strings ifLabel ":"))
				(code-gen dif constants-table major fvars-table)
				(print-tabbed-line (concat-strings ifendLabel ":")))
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
			(print-line (string-append "
					;adding new-line to the new extended env
					SAFE_MALLOC " (number->string (* 8 (+ 1 major)))
					;mov rax, " (number->string (* 8 (+ 1 major))) ;mov rax, 8*(major+1)"
					;push rax
					;call our_malloc
					"
					;add rsp, 8
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

					;creating env' -> the new extended env
					mov rdx, [rbp+8*3] 		;rdx <-- n
					shl rdx, 3 				;rdx <-- n*8
					SAFE_MALLOC rdx
					;push rax
					;call our_malloc
					;add rsp, 8
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
					;mov rax, 16
					;push rax
					;call our_malloc
					SAFE_MALLOC 16
					;add rsp, 8
					MAKE_LITERAL_CLOSURE rax, rbx, " labelLambda"
					;mov rax, [rax]
					jmp " labelEndLambda"
					"labelLambda":
					push rbp
					mov rbp, rsp
					"))
			(code-gen body constants-table (+ 1 major) fvars-table)
			(print-line (string-append "
					mov rbx, [rbp+8*3] 		;rbx <-- n
					leave
					pop rcx					;rcx <-- ret
					add rbx, 3
					shl rbx, 3
					add rsp, rbx 			;clean stack
					push rcx
					ret 
					"labelEndLambda":
				"))
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
			(print-line (string-append "
				;pushing NIL
				push SOB_NIL
				;pushing arguments"))
			(my-map (lambda (param)
				(code-gen param constants-table major fvars-table) 
				(print-line "push rax"))
				params)
			(print-line (string-append "
				;push n
				push " (number->string n)))
			(code-gen proc constants-table major fvars-table)
			(print-line (string-append "
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
				;add rsp,  (number->string (* (+ 3 n) 8))"
				)))
	))

(define pvar-gen
	(lambda (pvar-body)
		(let ((minor (cadr pvar-body)))
			(print-line (string-append "
				mov rax, [rbp +" (number->string (* 8 (+ 4 minor))) "]
			"))
		)
	))

(define set-pvar-gen
	(lambda (set-pvar-body constants-table major fvars-table)
		(let ((minor (caddar set-pvar-body))
				(value (cadr set-pvar-body)))
			(code-gen value constants-table major fvars-table)
			(print-line (string-append "
				mov qword[rbp +" (number->string (* 8 (+ 4 minor))) "], rax
				mov rax, sobVoid
			"))
		)
	))

(define bvar-gen
	(lambda (bvar-body)
		(let ((major (cadr bvar-body))
				(minor (caddr bvar-body)))
			(print-line (string-append "
				mov rax, qword[rbp +" (number->string (* 8 2)) "] 	;env
				mov rax, qword[rax +" (number->string (* 8 major)) "] 	;env[major]
				mov rax, qword[rax +" (number->string (* 8 minor)) "]	;env[major][minor]
			"))
		)
	))

(define set-bvar-gen
	(lambda (set-bvar-body constants-table major fvars-table)
		(let* ((major-minor (cddar set-bvar-body))
				(major (car major-minor))
				(minor (cadr major-minor))
				(value (cadr set-bvar-body)))
			(code-gen value constants-table major fvars-table)
			(print-line (string-append "
				mov rbx, qword[rbp+2*8]
				mov rbx, qword[rbx +" (number->string (* 8 major)) "]
				mov qword[rbx +" (number->string (* 8 minor)) "], rax
				mov rax, sobVoid
			"))
		)
	))

(define fvar-gen
	(lambda (fvar fvars)
		(let ((label (lookup-fvar-get-label fvar fvars)))
			(print-tabbed-line (concat-strings "mov rax, [" label "] ;fvar-gen"))
			)
))

(define set-fvar-gen
	(lambda (set-fvar-body constants-table major fvars-table)
		(let* ((fvar-name (cadar set-fvar-body))
			   (value (cadr set-fvar-body))
			   (label (lookup-fvar-get-label fvar-name fvars-table)))
			(code-gen value constants-table major fvars-table)
			(print-tabbed-line (string-append "mov [" label "], rax ; set-fvar
				mov rax, sobVoid" ))
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
				(labelLoopFixStack (loop-fix-stack-label))
				(labelLoopFixStackEnd (loop-fix-stack-end-label)))
			(print-line (string-append "
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

					;creating env' -> the new extended env
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



					;FIXING STACK
					mov r8, sobNil
					mov rdi, [rbp+8*3]					;rdi <-- n
					
					mov r9, "(number->string (+ (length params) 1))" 	;t9 <-- |params|
					"
 					labelLoopFixStack":
 					cmp rdi, r9
 					jl "labelLoopFixStackEnd "
 					mov r10, [rbp+8*(rdi+3)] 			;param[rdi]
 					"(debug-label)":
 					MAKE_PAIR r10, r8
 					SAFE_MALLOC 8
 					mov [rax], r10
 					mov r8, rax 
 					dec rdi
 					jmp "labelLoopFixStack "
 					"
 					labelLoopFixStackEnd":
 					mov [rbp+8*(r9+3)], r8
					"))

			(code-gen body constants-table (+ 1 major) fvars-table)
			(print-line (string-append "
					mov rbx, [rbp+8*3] 		;rbx <-- n
					leave
					pop rcx					;rcx <-- ret
					add rbx, 3
					shl rbx, 3
					add rsp, rbx 			;clean stack
					push rcx
					ret 
					"labelEndLambda":
				"))
		)
	))

(define box-gen
	(lambda (box-body constants-table major fvars-table)
		(code-gen box-body constants-table major fvars-table)
		(print-line (string-append "
			mov rbx, rax
			SAFE_MALLOC 8
			mov qword[rax], rbx
			"))
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
			(print-line (string-append "
				;pushing NIL
				push SOB_NIL
				;pushing arguments"))
			(my-map (lambda (param)
				(code-gen param constants-table major fvars-table) 
				(print-line "push rax"))
				params)
			(print-line (string-append "
				;push m
				push " (number->string m)))
			(code-gen proc constants-table major fvars-table)
			(print-line (string-append "
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
				add r9, 4
				shl r9, 3				;r9 <-- (n+4)*8
				add r9, r8				;r9 <-- r8 + (n+4)*8
				mov r10, r9				;previous NIL 
				sub r8, 8				;current NIL
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

				add r11, 5				;r11 <-- 5+n
				shl r11, 3				;r11 <-- 8*(5+n)
				add rsp, r11
				jmp rax"
				)))

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
			(map (lambda (fvar) 
				(print-line (string-append (cadr fvar) ":
					dq SOB_UNDEFINED")))
				fvars)
	))



(define gen-code-assembly
	(lambda (asts constants-table fvars)
		(if (null? asts) void
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

				(prologue-assembly (gen-prologue-assembly))
				(constants-assembly (gen-constants-assembly constants-table))
				(fvars-assembly (gen-fvars-assembly fvars))
				(section-bss (gen-section-bss))
				(section-text (gen-section-text))
				(funcs (init-functions fvars))
				(fake-env (gen-fake-env))
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
		(let* ((asts (append scheme-expressions (pipeline (file->list file))))
			   (const-table (build-constants-table asts))
			   (fvars-table (label-fvars '() (get-fvars-list asts)))
			   )
		;asts
		;fvars-table
		;const-table
		;void
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

;unique labels generator
(define ^make_label
  (lambda (perfix)
    (let ((n 0))
      (lambda ()
        (set! n (+ n  1))
        (format "~A_~A" perfix n))))
    )

(define or-label
	(^make_label "Lor"))

(define fvar-label
    (^make_label "Lglob"))

(define if-label
    (^make_label "Lif"))

(define if-end-label
    (^make_label "Lifend"))

(define loop-label
	(^make_label "Lloop"))

(define loop-end-label
	(^make_label "Lloopend"))

(define lambda-label
	(^make_label "Llambda"))

(define lambda-end-label
	(^make_label "LlambdaEnd"))

(define debug-label
	(^make_label "Ldebug"))

(define glob-label
	(^make_label "Lglob"))

(define cons-label
	(^make_label "LconsStart"))

(define cons-end-label
	(^make_label "LconsEnd"))

(define loop-fix-stack-label
	(^make_label "LloopFixStack"))

(define loop-fix-stack-end-label
	(^make_label "LloopFixStackEnd"))

(define tc-loop-label
	(^make_label "Ltc_loop"))

(define tc-loop-end-label
	(^make_label "Ltc_loop_end"))