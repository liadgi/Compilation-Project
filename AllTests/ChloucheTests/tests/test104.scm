(define x '(1 2)) (define y '(3 4)) (define z (append x y)) (set-car! x '*) (set-car! y '$) z                                           ; (1 . (2 . ($ . (4 . ()))))
