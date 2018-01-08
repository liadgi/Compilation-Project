(let ((a 0))
  (list
  (lambda () a)
  (lambda () (set! a (+ a 1)))
  (lambda (b) (set! a b))))