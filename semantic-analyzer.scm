(load "tag-parser.scm")

(define applic-lambda-nil? 
	(lambda (pexpr)
		(and 	
			(list? pexpr)
			(eq? (car pexpr) 'applic) 
			(eq? (caadr pexpr) 'lambda-simple)
			(null? (caddr pexpr))
			(null? (cadadr pexpr)))
))

(define remove-applic-lambda-nil
 	(lambda (pexpr)
		(cond 	((null? pexpr) pexpr)	
				((applic-lambda-nil? pexpr)
					(remove-applic-lambda-nil (car (cddadr pexpr))))				
				((list? pexpr)
					(cons (remove-applic-lambda-nil (car pexpr)) (remove-applic-lambda-nil (cdr pexpr)))) 
				(else pexpr))
 	) 
)

(define lambda?
	(lambda (expr)
		(and (list? expr) (eq? (car expr) 'lambda-simple))
))

(define lambdaopt?
	(lambda (expr)
		(and (list? expr) (eq? (car expr) 'lambda-opt))
))

(define in-body?
	(lambda (p body)
		(cond	((or (null? body) (not (list? body))) #f)
				((lambda? body) (free? body p))
				((lambdaopt? body) (free-opt? body p))
				((and (eq? (car body) 'var) (eq? (cadr body) p)) #t)
				(else (or (in-body? p (car body)) (in-body? p (cdr body)))))
))

(define free? 
	(lambda (lambdaExp param)
		(let 	((params (cadr lambdaExp))
				(body (caddr lambdaExp)))
		(if (member param params) #f (in-body? param body)))
))

(define free-opt? 
	(lambda (lambdaExp param)
		(let 	((params `(,@(cadr lambdaExp) ,(caddr lambdaExp)))
				(body (cadddr lambdaExp)))
			(and
				(not (member param params))
				(in-body? param body)))
))

(define has-bound? 
	(lambda (body param)
		(cond 	((or (null? body) (not (list? body))) #f)
				((lambda? body) (free? body param))
				((lambdaopt? body) (free-opt? body param))
				((list? body) (or (has-bound? (car body) param) (has-bound? (cdr body) param)))
				(else #f))
))

(define is-set?
	(lambda (body param)
		(cond	((or (null? body) (not (list? body))) #f)
				((and (or (lambda? body) (lambdaopt? body)) (member param (cadr body))) #f)
				((and (list? body) (eq? (car body) 'set) (equal? (cadr body) `(var ,param))) #t)
				((list? body) (or (is-set? (car body) param) (is-set? (cdr body) param)))
				(else #f)
		)
))

(define has-get?
	(lambda (body param)
		(cond 	((or (null? body) (not (list? body))) #f)
				((and (or (lambda? body) (lambdaopt? body)) (member param (cadr body))) #f)
				((not (list? body)) #f)
				((eq? (car body) 'set) (has-get? (caddr body) param))
				((equal? body `(var ,param)) #t)
				(else (or (has-get? (car body) param) (has-get? (cdr body) param)))
		)
))

(define should-box?
	(lambda (body param)
		(and 
			(has-bound? body param)
			(is-set? body param)
			(has-get? body param)
		)
))

(define add-box-set
	(lambda (body param)
		(cond 	((or (null? body) (not (list? body))) body)
				((and (eq? (car body) 'set) (equal? (cadr body) `(var ,param))) 
					`(box-set ,(cadr body) ,(add-box-set (caddr body) param)))
				((and (lambda? body) (member param (cadr body))) body)
				((and (lambdaopt? body) (member param `(,(cadr body) ,(caddr body)))) body)
				(else (cons (add-box-set (car body) param) (add-box-set (cdr body) param)))
		)
))

(define add-box-get
	(lambda (body param)
		(cond 	((or (null? body) (not (list? body))) body)
				((eq? (car body) 'box-set) `(,(car body) ,(cadr body) ,(add-box-get (caddr body) param)))
				((eq? (car body) 'set) `(,(car body) ,(cadr body) ,(add-box-get (caddr body) param)))
				((and (lambda? body) (member param (cadr body))) body)
				((and (lambdaopt? body) (member param `(,(cadr body) ,(caddr body)))) body)
				((equal? body `(var ,param)) `(box-get (var ,param)))
				(else (cons (add-box-get (car body) param) (add-box-get (cdr body) param)))
		)		
))

(define box-body
	(lambda (body param)
		(add-box-get (add-box-set body param) param)
))

(define box-params
	(lambda (params body toAdd)
		(cond ((null? params) (if (null? toAdd) body 
					(if (eq? (car body) 'seq) `(seq (,@(reverse toAdd) ,@(cadr body)))
						`(seq (,@(reverse toAdd) ,body)))))
			((should-box? body (car params)) 
				(box-params (cdr params) (box-body body (car params)) 
					(cons `(set (var ,(car params)) (box (var ,(car params)))) toAdd)))
			(else (box-params (cdr params) body toAdd))
		)
)) 

(define box-set
	(lambda (expr)
		(cond 	((or (null? expr) (not (list? expr))) expr)
				((lambda? expr) `(,(car expr) ,(cadr expr) 
					,(box-params (cadr expr) (box-set (caddr expr)) '())))
				((lambdaopt? expr) `(,(car expr) ,(cadr expr) ,(caddr expr) 
					,(box-params (append (cadr expr) `(,(caddr expr))) (box-set (cadddr expr)) '())))
				(else (cons (box-set (car expr)) (box-set (cdr expr))))
			)

))

(define get-index
	(lambda (x lst counter)
		(cond ((null? lst) #f)
			((eq? (car lst) x) counter)
			(else (get-index x (cdr lst) (+ counter 1)))
		)
))

(define tag-var
	(lambda (body params)
		(cond ((or (null? body) (not (list? body))) body)
				((and (eq? (car body) 'var) (get-index (cadr body) params 0)))
		)
))

(define get-bound-index
	(lambda (var scope major)		
		(if (null? scope) #f
				(let ((minor (get-index var (car scope) 0)))
					(if minor (cons major minor)
						(get-bound-index var (cdr scope) (+ major 1)))))
))

(define analyse
	(lambda (expr scope params)
		(cond ((or (null? expr) (not (list? expr))) expr)
			((lambda? expr) 
				`(,(car expr) ,(cadr expr) ,(analyse (caddr expr) (cons params scope) (cadr expr))))
			((lambdaopt? expr)
				(let 	((curr-params `(,@(cadr expr) ,(caddr expr)))
						(body (cadddr expr)))
					`(,(car expr) ,(cadr expr) ,(caddr expr) ,(analyse body (cons params scope) curr-params))))
			((eq? (car expr) 'var)
				(let* 	((curr-var (cadr expr))
						(pvar-ind (get-index curr-var params 0)))
					(if pvar-ind `(pvar ,curr-var ,pvar-ind)
						(let ((bound-index (get-bound-index curr-var scope 0)))
							(if bound-index `(bvar ,curr-var ,(car bound-index) ,(cdr bound-index))
								`(fvar ,curr-var))))))
			(else (cons (analyse (car expr) scope params) (analyse (cdr expr) scope params))))
))

(define pe->lex-pe
  	(lambda (expr)
  		(analyse expr '() '())
))

(define or-helper
	(lambda (expr in-tp?)
		(if (null? (cdr expr)) (cons (annotate (car expr) in-tp?) '())
			(cons (annotate (car expr) #f) (or-helper (cdr expr) in-tp?)))
))

(define annotate-or-seq
	(lambda (expr in-tp?)
		`(,(car expr) ,(or-helper (cadr expr) in-tp?))
))

(define annotate-if
	(lambda (expr in-tp?)
		(let ((test (cadr expr))
			(dit (caddr expr))
			(dif (cadddr expr)))
		(list (car expr) (annotate test #f) (annotate dit in-tp?) (annotate dif in-tp?)))
))

(define annotate-set
	(lambda (expr in-tp?)
		(list (car expr) (cadr expr) (annotate (caddr expr) #f))
))

(define annotate-box
	(lambda (expr in-tp?)
		`(,(car expr) ,(annotate (cadr expr) in-tp?))
))

(define annotate-lambda
	(lambda (expr in-tp?)
		(if (lambda? expr)
			(list (car expr) (cadr expr) (annotate (caddr expr) #t))
			(list (car expr) (cadr expr) (caddr expr) (annotate (cadddr expr) #t)))
))

(define annotate-applic
	(lambda (expr in-tp?)
		(if in-tp?
			`(tc-applic ,(annotate (cadr expr) #f) ,(map (lambda (x) (annotate x #f)) (caddr expr)))
			`(,(car expr) ,(annotate (cadr expr) #f) ,(map (lambda (x) (annotate x #f)) (caddr expr))))
))

(define annotate
	(lambda (expr in-tp?)
		(cond 
			((or (null? expr) (not (list? expr))) expr)
			((or (eq? (car expr) 'const) (eq? (car expr) 'pvar) (eq? (car expr) 'bvar) (eq? (car expr) 'fvar)) expr)
			((or (eq? (car expr) 'or) (eq? (car expr) 'seq)) (annotate-or-seq expr in-tp?))
			((eq? (car expr) 'if3) (annotate-if expr in-tp?))
			((or (eq? (car expr) 'define) (eq? (car expr) 'set) (eq? (car expr) 'box-set)) 
				(annotate-set expr in-tp?))
			((or (eq? (car expr) 'box) (eq? (car expr) 'box-get)) (annotate-box expr 'in-tp?))
			((or (eq? (car expr) 'lambda-simple) (eq? (car expr) 'lambda-opt)) (annotate-lambda expr in-tp?))
			((eq? (car expr) 'applic) (annotate-applic expr in-tp?))
			(else expr)
		)
))

(define annotate-tc
	(lambda (expr)
		(annotate expr #f)
))