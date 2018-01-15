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


(define gen-prolog-assembly
	(lambda ()
		(print-line "section .data")
		(print-line "start_of_data:")
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
							  ;((eq? type 'T_INT) (gen-make-literal-integer value address))
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
))

(define gen-section-text
	(lambda ()
		(print-line "")
		(print-line "section .text")
		(print-line "main:")
))

(define const-gen
	(lambda (value const-table)
		(let* ((const-label (lookup-constant-get-label value const-table)))
			(print-tabbed-line (concat-strings "mov rax, [" const-label "]"))
			)
))

(define gen-code-assembly
	(lambda (ast constants-table)
		(if (null? ast) void
			(begin
				(cond ((eq? (caar ast) 'const) (const-gen (cadar ast) constants-table)) 
					  )
				(gen-code-assembly (cdr ast) constants-table)
			)
		)
		))

;(define code-gen

(define frame-gen
	(lambda (structure)
		(let* ((constants-table (car structure))

				(prolog-assembly (gen-prolog-assembly))
				(constants-assembly (gen-constants-assembly constants-table))
				(section-bss (gen-section-text))
				(code-assembly (gen-code-assembly (cadr structure) constants-table))
				)
			constants-assembly
			)
		)
)

(define output-port #f)

(define compile-scheme-file
	(lambda (file output-file)
		(let* ((ast (pipeline (file->list file)))
			   (const-table (build-constants-table ast))
			   )
		;ast
		;const-table
		(begin 
			(delete-file output-file) 
			(set! output-port (open-output-file output-file))
			(frame-gen (list const-table ast))
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



(define all-consts
	(lambda (lst)
		(cond ((null? lst) lst)
			((number? (car lst)) (append `(,(car lst)) (all-consts (cdr lst))))
			((list? (car lst)) (append (all-consts (car lst)) (parse-list-helper (car lst)) (all-consts (cdr lst)) ))
			((vector? (car lst)) (append (all-consts (vector->list (car lst))) `(,(car lst)) (all-consts (cdr lst))))
			)
))

(define parse-list-helper 
	(lambda (lst)
		(cond ((null? lst) lst)
			((number? lst) lst)
			((list? lst) `(,@(parse-list-helper (cdr lst)) ,lst))

			)
))