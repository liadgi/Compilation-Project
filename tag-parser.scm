(load "qq.scm")

(define *reserved-words*
	'(and begin cond define do else if lambda
	let let* letrec or quasiquote unquote
	unquote-splicing quote set!))


(define const?
	(lambda (const) 
		(or 
			(number? const)
			(boolean? const)
			(vector? const)
			(string? const)
			(char? const)
			(eq? const (void))
)))

(define var?
	(lambda (sexpr)
		(and
			(symbol? sexpr)
			(not (member sexpr *reserved-words*)))
))


(define if3?
	(lambda (sexpr)
		(and (eq? (car sexpr) 'if)
			(eq? (length sexpr) 4))
))

(define if2?
	(lambda (sexpr)
		(and (eq? (car sexpr) 'if)
			(eq? (length sexpr) 3))
))

(define or?
	(lambda (sexpr)
		(eq? (car sexpr) 'or)
))

(define or-handler
	(lambda (sexpr)
		(cond ((null? sexpr) (parse #f))
			  ((eq? (length sexpr) 1) (parse (car sexpr)))
			  (else `(or ,(map parse sexpr))))
))

; not working yet because 'and' is not implemented
#;(parse '(or (or (f1 x) (f2 y))
	(or (f3 z) (f4 w) (f5 r))
	(and (f6 u) (f7 t))))

(define is-lambda?
	(lambda (sexpr)
		(and (eq? (car sexpr) 'lambda)
			 (> (length (cdr sexpr)) 1))
))

(define lambda-simple?
	(lambda (sexpr)
		(and (is-lambda? sexpr)
			 (list? (cadr sexpr)))
))

(define lambda-simple-handler
	(lambda (exprs)
		;(if (eq? (length exprs) 2)
		;	`(lambda-simple ,(car exprs) ,(parse (cadr exprs))) 
			`(lambda-simple ,(car exprs) ,(parse `(begin ,@(cdr exprs))));)
))

(define lambda-opt?
	(lambda (sexpr)
			(and 
				(is-lambda? sexpr)
				(pair? (cadr sexpr))
				(not (list? (cadr sexpr))))
))

(define get-opt-params
	(lambda (params)
		(letrec ((run 
					(lambda (aggr rest) 
						(if (not (pair? rest)) 
							(cons aggr rest)
							(run (append aggr (list (car rest))) (cdr rest)))
						)))
			(run '() params))
		)
	)


(define lambda-opt-handler
	(lambda (exprs)
		(let ((params (car (get-opt-params (car exprs))))
			  (opt-params (cdr (get-opt-params (car exprs)))))
		;(if (eq? (length exprs) 2)
		;	`(lambda-opt ,params ,opt-params ,(parse (cadr exprs))) 
			`(lambda-opt ,params ,opt-params ,(parse `(begin ,@(cdr exprs)))));)
))


(define is-variadic-lambda?
	(lambda (sexpr)
		(and (is-lambda? sexpr)
			 (not (pair? (cadr sexpr))))
))

(define lambda-variadic-handler
	(lambda (exprs)
		(lambda-opt-handler exprs)
))

(define define?
	(lambda (sexpr)
		(and 
			(eq? (car sexpr) 'define)
			(eq? (length sexpr) 3)
			(not (pair? (cadr sexpr)))
			)
))

(define mit-define?
	(lambda (sexpr)
		(and 
			(eq? (car sexpr) 'define)
			(> (length sexpr) 2)
			(pair? (cadr sexpr))
			(not (null? (cadr sexpr)))
			)
))

(define mit-define-handler
	(lambda (sexpr)
		(let ((var-name (caar sexpr))
			  (params (cdar sexpr))
			  (exps (cdr sexpr)))

		`(define ,(parse var-name) ,(parse `(lambda ,params ,@exps)))
		)
))

(define set?
	(lambda (sexpr)
		(eq? (car sexpr) 'set!)
))

(define application?
	(lambda (sexpr)
		(and 
			(not (null? sexpr))
			(not (member (car sexpr) *reserved-words*)))
))

(define application-handler
	(lambda (sexpr)
		`(applic ,(parse (car sexpr)) ,(map parse (cdr sexpr)))
))


(define explicit-sequence?
	(lambda (exprs)
		(eq? (car exprs) 'begin)))

(define flat-begin
	(lambda (exprs)
		(if (null? exprs)		
			exprs
			(if (and (list? (car exprs)) (explicit-sequence? (car exprs))) (flat-begin (append (cdar exprs) (cdr exprs)))
				(append (list (car exprs)) (flat-begin (cdr exprs)))
			)
		)
))

(define begin-handler
	(lambda (exprs)
		(let ((flatted (flat-begin exprs)))
			(cond ((null? flatted) (parse (void)))
				((null? (cdr flatted)) (parse (car flatted)))
				(else `(seq ,(map parse flatted)))
			))
))

#;(define let-param?
	(lambda (sexpr)
		(and
			(pair? sexpr) ; '(a 3)
			(list? (cdr sexpr)) ; not '(a . 3)
			(eq? (length sexpr) 2) ; not '(a b 3)
			(symbol? (car sexpr)) ; is first elem valid
			;(not (symbol? (cadr sexpr))) ; check second elem for cases such as '(a b), '(a define), '(a ())
			)
))

#;(define let-param-names-unique?
	(lambda (sexpr)
		(letrec ((unique? 
					(lambda (agg-vars rest)
							(if (null? rest)
								 #t
								 (and (not (member (caar rest) agg-vars))
								   	  (unique? (append agg-vars (list (caar rest))) (cdr rest)))
					))
				))
		(unique? (list (caar sexpr)) (cdr sexpr)))
))

#;(define let-params?
	(lambda (sexpr)
		(and
			(list? sexpr) ; params is list
			(andmap let-param? sexpr) ; each param is valid
			(let-param-names-unique? sexpr) ; each param is unique
			)
))



#;(define let-body?
	(lambda (sexpr)
		(not (null? sexpr))
))


#;(define let-first-form?
	(lambda (sexpr)
		(and
			(pair? (cdr sexpr)) ; no "'(let EMPTY)" or "'(let* EMPTY)"
			(let-params? (cadr sexpr))
			(let-body? (cddr sexpr))
			)
))

; add let form - (let () 5)
; another let form (racket doc) - (let procname (x y) y)
#;(define let?
	(lambda (sexpr)
		(and
			(eq? (car sexpr) 'let) 
			(or
				(let-first-form? sexpr)
				;(let-second-form? sexpr)
			))
))

#;(define get-let-vars
	(lambda (sexpr)
		(map car (cadr sexpr))
))

#;(define get-let-exps
	(lambda (sexpr)
		(cddr sexpr)
))

#;(define get-let-args
	(lambda (sexpr)
		(map cadr (cadr sexpr))
))

; check for hygiene

#;(define let-handler
	(lambda (sexpr)
		(let ((vars (get-let-vars sexpr))
			  (exps (get-let-exps sexpr))
			  (args (get-let-args sexpr)))
			(parse 
				`((lambda ,vars ,@exps ) ,@args)
			)
		)
))

#;(define let*?
	(lambda (sexpr)
		(and
			(eq? (car sexpr) 'let*) 
			(let-first-form? sexpr))
))

#;(define let*-handler
	(lambda (sexpr)
		(let ((first-param (caadr sexpr))
			  (rest-params (cdadr sexpr))
			  (exps (cddr sexpr)))
			(parse 
				(if (null? rest-params)
				`(let (,first-param) ,@exps)
				`(let (,first-param) (let* ,rest-params ,@exps)))
			)
		)

))

; tests
;(parse '(let ((a 3)) c))
;(parse '(let ((a 3) (b 5) (c 9)) c d))

#;(define letrec?
	(lambda (sexpr)
		(and
			(eq? (car sexpr) 'letrec) 
			(let-first-form? sexpr))
))

#;(define create-letrec-vars
	(lambda (sexpr)
		(map (lambda (param) `(,(car param) 'dc)) (cadr sexpr))
))

#;(define create-letrec-sets
	(lambda (sexpr)
		(map (lambda (param) `(set! ,(car param) ,(cadr param))) (cadr sexpr))
))

#;(define letrec-handler
	(lambda (sexpr)
		(let ((dc-vars (create-letrec-vars sexpr))
			  (sets (create-letrec-sets sexpr))
			  (exps (get-let-exps sexpr))
			  )
			(parse 
				`(let ,dc-vars ,@sets ,@exps)
			)
		)

))

(define unique?
	(lambda (lst)
		(if (null? lst) #t
			(if (member (car lst) (cdr lst)) #f
				(unique? (cdr lst)))
		)
))

(define let?
	(lambda (sexpr)
		(and
			(eq? (car sexpr) 'let)
			(list? (cadr sexpr))
			(andmap (lambda (lst) (and (list? lst) (equal? (length lst) 2))) (cadr sexpr))
			(unique? (map car (cadr sexpr)))
		)
))

(define let-handler
	(lambda (exprs)
		(let (
			(params (map car (car exprs)))
			(args (map cadr (car exprs)))
			(body (cdr exprs)))
			`((lambda ,params ,@body) ,@args))
))


(define let*?
	(lambda (sexpr)
		(and
			(eq? (car sexpr) 'let*)
			(list? (cadr sexpr))
			(andmap (lambda (lst) (and (list? lst) (equal? (length lst) 2))) (cadr sexpr))
		)
))

(define let*-handler
	(lambda (exprs)
		(if (null? (car exprs)) `(let () ,@(cdr exprs))
			(let*-with-params-handler exprs))
))

(define let*-with-params-handler
	(lambda (exprs)
		(if (null? (cdar exprs)) `(let (,(caar exprs)) ,@(cdr exprs))
			`(let (,(caar exprs)) ,(let*-with-params-handler (cons (cdar exprs) (cdr exprs)))))	
))

(define letrec?
	(lambda (sexpr)
		(and
			(eq? (car sexpr) 'letrec)
			(list? (cadr sexpr))
			(andmap (lambda (lst) (and (list? lst) (equal? (length lst) 2))) (cadr sexpr))
			(unique? (map car (cadr sexpr)))
		)
))

(define letrec-handler
	(lambda (exprs)
		`(let (,@(map (lambda (exp) (list exp #f)) (map car (car exprs))))
			,@(map (lambda (exp arg) (list 'set! exp arg)) (map car (car exprs)) (map cadr (car exprs)))
			((lambda () ,@(cdr exprs))))
))








(define and?
	(lambda (sexpr)
		(eq? (car sexpr) 'and)
))

(define and-handler
	(lambda (exprs)
		(if (null? exprs) #t
			(two-params-and-handler exprs)
		)
))

(define two-params-and-handler
	(lambda (exprs)
		(if (null? (cdr exprs)) (car exprs)
		`(if ,(car exprs) ,(two-params-and-handler (cdr exprs)) #f))
))

(define cond?
	(lambda (sexpr)
		(and (eq? (car sexpr) 'cond)
			(list? (cadr sexpr))
		)
))

(define cond-handler
	(lambda (exprs)
		(if (eq? (caar exprs) 'else)  ;;last condition or else clause
			`(begin ,@(cdar exprs))
			(if (null? (cdr exprs))
				`(if ,(caar exprs) (begin ,@(cdar exprs)))
				`(if ,(caar exprs) (begin ,@(cdar exprs)) ,(cond-handler (cdr exprs))))
		)
))


(define qq? (^quote? 'quasiquote))

(define parse
	(lambda (sexpr)
		(cond ((const? sexpr) `(const ,sexpr))
				((quote? sexpr) `(const ,@(cdr sexpr)))
				((var? sexpr) `(var ,sexpr))
				((list? sexpr) 
					(cond 	((if3? sexpr) `(if3 ,(parse (cadr sexpr)) ,(parse (caddr sexpr)) ,(parse (cadddr sexpr))))
							((if2? sexpr) `(if3 ,(parse (cadr sexpr)) ,(parse (caddr sexpr)) ,(parse (void))))
							((or? sexpr) (or-handler (cdr sexpr)))
							((lambda-simple? sexpr) (lambda-simple-handler (cdr sexpr)))
							((lambda-opt? sexpr) (lambda-opt-handler (cdr sexpr)))
							((is-variadic-lambda? sexpr) (lambda-variadic-handler (cdr sexpr)))
							((define? sexpr) `(define ,(parse (cadr sexpr)) ,(parse (caddr sexpr))))
							((mit-define? sexpr) (mit-define-handler (cdr sexpr)))
							((set? sexpr) `(set ,(parse (cadr sexpr)) ,(parse (caddr sexpr))))
							((application? sexpr) (application-handler sexpr))
							((explicit-sequence? sexpr) (begin-handler (cdr sexpr)))
							((let? sexpr) (parse (let-handler (cdr sexpr))))
							((let*? sexpr) (parse (let*-handler (cdr sexpr))))
							((letrec? sexpr) (parse (letrec-handler (cdr sexpr))))
							((and? sexpr) (parse (and-handler (cdr sexpr))))
							((cond? sexpr) (parse (cond-handler (cdr sexpr))))
							((qq? sexpr) (parse (expand-qq (cadr sexpr))))
							(else (format "Error: list but not tagged yet: ~a ." sexpr)))
				)
				(else "error")
		)
		)
)