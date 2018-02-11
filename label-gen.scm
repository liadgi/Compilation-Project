;unique labels generator
(define ^make_label
  (lambda (perfix)
    (let ((n 0))
      (lambda ()
        (set! n (+ n  1))
        (format "~A_~A" perfix n))))
    )

(define or-label
	(^make_label "Lor"))

(define fvar-label
    (^make_label "Lglob"))

(define if-label
    (^make_label "Lif"))

(define if-end-label
    (^make_label "Lifend"))

(define loop-label
	(^make_label "Lloop"))

(define loop-end-label
	(^make_label "Lloopend"))

(define lambda-label
	(^make_label "Llambda"))

(define lambda-end-label
	(^make_label "LlambdaEnd"))

(define debug-label
	(^make_label "Ldebug"))

(define glob-label
	(^make_label "Lglob"))

(define loop-fix-stack-label
	(^make_label "LloopFixStack"))

(define loop-fix-stack-end-label
	(^make_label "LloopFixStackEnd"))

(define tc-loop-label
	(^make_label "Ltc_loop"))

(define tc-loop-end-label
	(^make_label "Ltc_loop_end"))

(define lambda-less-args-than-params-label
	(^make_label "Llambda_less_args_than_params"))

(define apply-label
	(^make_label "L_apply"))

(define apply-end-label
	(^make_label "L_apply_end"))