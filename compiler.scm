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

(define print-line
	(lambda (line)
		(concat-strings line) 
		))

(define gen-const-table
	(lambda (table)
		(letrec ((gen (lambda (table agg-code)
			(if (null? table) 
				agg-code
				(gen (cdr table) (string-append agg-code
					
					(let* ((first-pair (car table))
						   (value (car first-pair))
						   (address (cadr first-pair))
						   (type (caddr first-pair))
						   )
						(cond ((eq? type 'T_NIL) (print-line "dq SOB_NIL "))
							  ((eq? type 'T_INT) (print-line (concat-strings "dq MAKE_LITERAL(T_INTEGER, " (number->string value) ") ")))
							  (else "DO_LATER"))

						)

					))
				))))
		
		(gen table "")
		)
))

(define code-gen
	(lambda (structure)
		(gen-const-table (car structure))
		)
)

(define compile-scheme-file
	(lambda (file)
		(let* ((ast (pipeline (file->list file)))
			   (const-table (build-constants-table ast)))
			(write-to-target-file (code-gen (list const-table ast))
				)))
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
	(lambda (assembly-code)
		(let* ((none (delete-file "target.asm"))
			   (output-port (open-output-file "target.asm")))
			(write assembly-code output-port)
			(close-output-port output-port)
		))
)

