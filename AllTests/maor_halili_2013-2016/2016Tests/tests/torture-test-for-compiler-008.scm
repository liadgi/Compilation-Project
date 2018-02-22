;;; torture-test-for-compiler-008.scm
;;; << WHAT DOES THIS FILE CONTAIN >>
;;;
;;; Programmer: Mayer Goldberg, 2012

(define with (lambda (s f) (apply f s)))

(define fact
  (letrec ((fact-1
	    (lambda (n r)
	      (if (zero? n)
		  r
		  (fact-2 (- n 1)
			  (* n r)
			  'moshe
			  'yosi))))
	   (fact-2
	    (lambda (n r _1 _2)
	      (if (zero? n)
		  r
		  (fact-3 (- n 1)
			  (* n r)
			  'dana
			  'michal
			  'olga
			  'sonia))))
	   (fact-3
	    (lambda (n r _1 _2 _3 _4)
	      (if (zero? n)
		  r
		  (fact-1 (- n 1)
			  (* n r))))))
    (lambda (n)
      (fact-1 n 1))))