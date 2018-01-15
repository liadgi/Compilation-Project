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

(define gen-const-label
	(lambda (type value)
		(concat-strings type value)
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
	(lambda (value address)
		(let ((strVal (number->string value)))
			(print-line (concat-strings "sobInt" strVal ":"))
			(print-tabbed-line (concat-strings "dq MAKE_LITERAL(T_INTEGER, " strVal ")"))
		)))

(define gen-make-pair
	(lambda (value)
		value
		))

(define gen-prolog-assembly
	(lambda ()
		"prolog"
		))

(define gen-make-literal-nil
	(lambda ()
		(print-line "sobNil:")
		(print-tabbed-line "dq SOB_NIL")
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
						   )
						(cond ((eq? type 'T_NIL) (gen-make-literal-nil))
							  ((eq? type 'T_INT) (gen-make-literal-integer value address))
							  (else "DO_LATER "))

						)
					(gen (cdr table))
					)

				)
				)))
		(gen table))
		
		
		)
)

(define code-gen
	(lambda (structure)
		(let* ((contstants-assembly (gen-constants-assembly (car structure)))
				(prolog-assembly (gen-prolog-assembly)))
			contstants-assembly
			)
		)
)

(define output-port #f)

(define compile-scheme-file
	(lambda (file output-file)
		(let* ((ast (pipeline (file->list file)))
			   (const-table (build-constants-table ast)))
		(begin 
			(set! output-port (open-output-file output-file))
			(code-gen (list const-table ast))
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

