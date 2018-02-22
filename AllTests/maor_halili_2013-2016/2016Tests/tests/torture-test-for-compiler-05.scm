;;; torture-test-for-compiler-05.scm
;;; Gensym tests
;;;
;;; Programmer: Mayer Goldberg, 2012

(define test
  (lambda ()
    (let* ((f (lambda (s)
		(string->symbol
		 (symbol->string s))))
	   (g1 (gensym))
	   (g2 (gensym))
	   (g3 (gensym)))
      (and (eq? g1 g1)
	   (eq? g2 g2)
	   (eq? g3 g3)
	   (not (eq? g1 g2))
	   (not (eq? g2 g3))
	   (not (eq? g1 g3))
	   (eq? (f g1)
		(f (f g1)))
	   (eq? (f (f g2))
		(f (f (f (f (f g2))))))
	   (not (eq? g1 (f g1)))
	   (not (eq? g3 (f (f (f (f (f g3)))))))
	   (eq? (f (f g2))
		(f (f (f (f (f (f (f (f g2)))))))))))))