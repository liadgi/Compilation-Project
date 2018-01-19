(load "sexpr-parser.scm")
(load "tag-parser.scm")
(load "semantic-analyzer.scm")
(load "constants.scm")

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
			(print-line "mov rax, 0")
			(print-line "push rax") ;fake n
			(print-line "push SOB_NIL") ;fake env
			(print-line "push rax") ;fake ret
			(print-line "push rbp")
			(print-line "mov rbp, rsp")
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
			(print-line "main:")
			(gen-fake-env))
))

(define write_sob
	(lambda ()
			(print-tabbed-line (concat-strings "push rax"))
			(print-tabbed-line "call write_sob_if_not_void")
			(print-tabbed-line "add rsp, 1*8")
))

(define const-gen
	(lambda (value const-table)
		(let* ((const-label (lookup-constant-get-label value const-table)))
			(print-tabbed-line (concat-strings "mov rax, [" const-label "]"))
			
			)
))

(define seq-gen ; e1,e2,...,en
	(lambda (elems const-table major)
		(if (null? (cdr elems))
					(gen-code-for-ast (car elems) const-table major) ; return en
					(begin
						(gen-code-for-ast (car elems) const-table major)
						(seq-gen (cdr elems) const-table)
						)
					)
))

(define or-gen
	(lambda (exprs jmpLabel constants-table major)
		(if (null? (cdr exprs)) 
				(begin
					(gen-code-for-ast (car exprs) constants-table major)
					(print-tabbed-line (concat-strings jmpLabel ":")))
				(begin 
					(gen-code-for-ast (car exprs) constants-table major)
					(print-tabbed-line "cmp rax, SOB_FALSE")
					(print-tabbed-line (concat-strings "jne " jmpLabel))
					(or-gen (cdr exprs) jmpLabel constants-table))
			)
		))

(define if-gen
	(lambda (exprs ifLabel ifendLabel constants-table major)
		(let ((test (car exprs))
				(dit (cadr exprs))
				(dif (caddr exprs)))
			(begin 
				(gen-code-for-ast test constants-table major)
				(print-tabbed-line "cmp rax, SOB_FALSE")
				(print-tabbed-line (concat-strings "je " ifLabel))
				(gen-code-for-ast dit constants-table major)
				(print-tabbed-line (concat-strings "jmp " ifendLabel))
				(print-tabbed-line (concat-strings ifLabel ":"))
				(gen-code-for-ast dif constants-table major)
				(print-tabbed-line (concat-strings ifendLabel ":")))
		)))

#;(define lambda-simple-gen
	(lambda (exprs constants-table major)
		(let ((params (car exprs))
				(body (cadr exprs))
				(labeloop (loop-label))
				(labelEndLoop (loop-end-label)))
			(begin
				;adding new-line to env
				(print-tabbed-line (concat-strings "mov rax, " (number->string (* 8 (+ 1 major))))) ;mov rax, 8*(major+1)
				(print-tabbed-line "push rax") 
				(print-tabbed-line "call our_malloc")
				(print-tabbed-line "mov rbx, rax") ;rbx <-- malloc(8*(major+1))
				
				(print-tabbed-line (concat-strings "mov rax, " (number->string major))) ;rax <-- major
				(print-tabbed-line (concat-strings "mov rdi, 0" (number->string major))) ;mov rdi, 0
				(print-tabbed-line (concat-strings labeloop ":")) ;Lloop_0:
				(print-tabbed-line "cmp rdi, rax") ;cmp rdi, major
				(print-tabbed-line (concat-strings "je " labelEndLoop)) ;je Lloopend_0
				;rbx[i+1] = env[i]
				(print-tabbed-line "mov rcx, ")
				(print-tabbed-line "inc rdi")
				(print-tabbed-line (concat-strings labelEndLoop ":")) ;Lloopend_0:

			)	
		)
	))

(define lambda-simple-gen
	(lambda (exprs constants-table major)
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
					mov rax, " (number->string (* 8 (+ 1 major)))" ;mov rax, 8*(major+1)
					push rax
					call our_malloc
					add rsp, 8
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
					mov rax, [rbp+8*3] 		;rax <-- n
					shl rax, 3 				;rax <-- n*8
					push rax
					call our_malloc
					add rsp, 8
					mov rcx, rax 			;rcx <-- malloc(8*n)
					mov rax, [rbp+8*3] 		;rax <-- n
					mov rdi, 0
					"
					labeloop2":
					mov rdx, [rbp+8*4]		;rdx <-- first param
					cmp rdi, rax
					je " labelEndLoop2"
						;rcx[i] = param[i]
					mov rdx, [rdx+8*rdi] 	;rdx <-- param[i]
					mov[rcx+8*rdi], rdx 	;rcx[i] <-- param[i]
					inc rdi
					"labelEndLoop2":

					mov [rbx], rcx
					mov rax, 16
					push rax
					call our_malloc
					add rsp, 8
					MAKE_LITERAL_CLOSURE rax, rbx, " labelLambda"
					mov rax, [rax]
					jmp " labelEndLambda"
					"labelLambda":
					push rbp
					mov rbp, rsp
					"))
			(gen-code-for-ast body constants-table (+ 1 major))
			(print-line (string-append "
					leave
					ret
					"labelEndLambda":
				"))
		)
	))

(define applic-gen
	(lambda (applic-body constants-table major)
		(let* 	((proc (car applic-body))
				(params (reverse (cadr applic-body)))
				(n (length params)))
			(print-line (string-append "
				;pushing NIL
				push SOB_NIL
				;pushing arguments"))
			(map (lambda (param) 
				(gen-code-for-ast param constants-table major) 
				(print-line "push rax"))
				params)
			(print-line (string-append "
				;push n
				push " (number->string n)))
			(gen-code-for-ast proc constants-table major)
			(print-line (string-append "
				mov rbx, rax 	;rbx <-- closure(?)
				TYPE rbx
				cmp rbx, T_CLOSURE
				jne L_error_cannot_apply_non_clos
				mov rbx, rax
				CLOSURE_ENV rbx
				push rbx
				CLOSURE_CODE rax
				call rax
				add rsp, " (number->string (* (+ 3 n) 8))
				)))
	))

(define pvar-gen
	(lambda (pvar-body constants-table major)
		(let ((minor (cadr pvar-body)))
			(print-line (string-append "
				mov rax, [rbp +" (number->string (* 8 (+ 4 minor))) "]
			"))

		)
	))

(define gen-code-for-ast
	(lambda (ast constants-table major)
		(cond ((eq? (car ast) 'const) (const-gen (cadr ast) constants-table)) 
			  ((eq? (car ast) 'seq) (seq-gen (cadr ast) constants-table major))
			  ((eq? (car ast) 'or) (or-gen (cadr ast) (or-label) constants-table major))
			  ((eq? (car ast) 'if3) (if-gen (cdr ast) (if-label) (if-end-label) constants-table major))
			  ((eq? (car ast) 'lambda-simple) (lambda-simple-gen (cdr ast) constants-table major))
			  ((eq? (car ast) 'applic) (applic-gen (cdr ast) constants-table major))
			  ((eq? (car ast) 'pvar) (pvar-gen (cdr ast) constants-table major))
		)
))

(define gen-code-assembly
	(lambda (asts constants-table)
		(if (null? asts) void
			(begin
				(begin (gen-code-for-ast (car asts) constants-table 0)
				 	   (write_sob))
				(gen-code-assembly (cdr asts) constants-table)
			)
		)
		))

;(define code-gen

(define frame-gen
	(lambda (structure)
		(let* ((constants-table (car structure))

				(prologue-assembly (gen-prologue-assembly))
				(constants-assembly (gen-constants-assembly constants-table))
				(section-bss (gen-section-bss))
				(section-text (gen-section-text))
				(code-assembly (gen-code-assembly (cadr structure) constants-table))
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
			   (const-table (build-constants-table asts))
			   )
		;asts
		;const-table
		;void
		(begin 
			(delete-file output-file) 
			(set! output-port (open-output-file output-file))
			(frame-gen (list const-table asts))
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