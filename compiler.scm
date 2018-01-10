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


(define code-gen
	(lambda (scheme-code)
		scheme-code)
)

(define compile-scheme-file
	(lambda (file)
		(code-gen 
			(pipeline (file->list file))))
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
		(let ((output-port (open-output-file "target.asm")))
			(write assembly-code output-port)
			(close-output-port output-port)
		))
)

