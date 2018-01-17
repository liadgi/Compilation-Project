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
		(print-tabbed-line "ret")
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

(define gen-section-text
	(lambda ()
		(print-line "")
		(print-line "section .text")
		(print-line "main:")
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
	(lambda (elems const-table)
		(if (null? (cdr elems))
					(gen-code-for-ast (car elems) const-table) ; return en
					(begin
						(gen-code-for-ast (car elems) const-table)
						(seq-gen (cdr elems) const-table)
						)
					)
))

(define gen-code-for-ast
	(lambda (ast constants-table)
		(cond ((eq? (car ast) 'const) (const-gen (cadr ast) constants-table)) 
			  ((eq? (car ast) 'seq) (seq-gen (cadr ast) constants-table)) 	  
					  )
))

(define gen-code-assembly
	(lambda (asts constants-table)
		(if (null? asts) void
			(begin
				(begin (gen-code-for-ast (car asts) constants-table)
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

