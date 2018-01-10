'(1 2 3)

'(1 2 3 4)
'(3 2 3 4)
;1
;2
;3
;(list 1 2 4)

(vector 1 2 3)
;(+ 1 2)
(let ((a 0) (b 1))
  (list
  (lambda () a)
  (lambda () (set! a (+ a 1)))
  (lambda (b) (set! a b))
  (lambda (c) (set! c "hello"))))

'(1 2 3)

;(define x 3)
;x 

;1

;2

;3
;(+ 3 4)