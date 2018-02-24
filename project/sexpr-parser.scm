(load "project/pc.scm")

(define <Boolean>
  (new
   (*parser (word-ci "#t"))
   (*pack (lambda (_) #t))

   (*parser (word-ci "#f"))
   (*pack (lambda (_) #f))
   
   (*disj 2)
  
   ;(*parser <any>)
   ;*not-followed-by
   
  done)
  )

(define <Whitespaces>
  ;(star
   (const (lambda (ch) (char<=? ch #\space)))
  ; )
  )

(define <VisibleSimpleChar>
  
  (const (lambda (ch) (char<? #\space ch)))
  )

(define <CharPrefix>
  (new 
   (*parser (word "#\\"))
   done))

(define <HexChar>
  (new 
  (*parser (range #\0 #\9))
  (*parser (range-ci #\a #\f))
  (*disj 2)
  done)
  )


(define <NamedChar>
  (new
  (*parser (word-ci "lambda"))
  (*pack (lambda (_) (integer->char 955)) )
  (*parser (word-ci "newline"))
  (*pack (lambda (_) #\newline) )
  (*parser (word-ci "nul"))
  (*pack (lambda (_) #\nul) )
  (*parser (word-ci "page"))
  (*pack (lambda (_) #\page) )
  (*parser (word-ci "return"))
  (*pack (lambda (_) #\return) )
  (*parser (word-ci "space"))
  (*pack (lambda (_) #\space) )
  (*parser (word-ci "tab"))
  (*pack (lambda (_) #\tab) )
  (*disj 7)
   done)
  )

(define <HexUnicodeChar>
  (new
   (*parser (char #\x))
   (*parser <HexChar>)
   *plus
   (*caten 2)
   (*pack-with 
      (lambda (x hex)
        (integer->char
          (string->number 
            (list->string hex)
            16))
      )
    )
   done)
  )

(define <Char>
  (new
   (*parser <CharPrefix>)
   (*parser <NamedChar>)
   (*parser <HexUnicodeChar>)
   (*parser <VisibleSimpleChar>)
   (*disj 3)
   (*caten 2)
   (*pack-with (lambda (sulamit data) data))
   done)
  )

(define <BiggerThanSpace>
  (const (lambda (ch) (char<? #\space ch)))
  )




(define <SymbolChar>
    (new 
  (*parser (range #\0 #\9))
  (*parser (range-ci #\a #\z))
  (*parser (char #\!))
  (*parser (char #\$))
  (*parser (char #\^))
  (*parser (char #\*))
  (*parser (char #\-))
  (*parser (char #\_))
  (*parser (char #\=))
  (*parser (char #\+))
  (*parser (char #\<))
  (*parser (char #\>))
  (*parser (char #\?))
  (*parser (char #\/))
  (*disj 14)
  done)
   
  )

(define <Symbol>
  (new
   (*parser <SymbolChar>)
   *plus
   (*pack (lambda (sym)
     (string->symbol (string-downcase (list->string sym)))))
   done)
  )


(define <clean_spaces>
  (lambda (<parser>)
     (new
      (*parser <Whitespaces>)
      (*parser <parser>)
      (*parser <Whitespaces>)
      (*caten 3)
      (*pack-with (lambda (first p second) p))
      done)
  ))



(define <ProperList>
  (new
   (*parser (char #\())
   (*delayed (lambda () <sexpr>))
   *star
   (*parser (char #\)))
   (*caten 3)
   (*pack-with (lambda (left lst right) lst))
   done)
  )

(define <ImproperList>
  (new
   (*parser (char #\())
   (*delayed (lambda () <sexpr>))
   *plus
   (*parser (char #\.))
   (*delayed (lambda () <sexpr>))
   (*parser (char #\)))
   (*caten 5)
   (*pack-with (lambda (left first dot second right) `(,@first ,@second)))
   
   done)
  )

(define <Vector>
  (new
   (*parser (word "#("))
   (*delayed (lambda () <sexpr>))
   *star
   (*parser (char #\)))
   (*caten 3)
   (*pack-with (lambda (left vec right) `#(,@vec)))
   done)
  )



(define <Natural>
  (new
    (*parser (range #\0 #\9)) *plus
    (*pack (lambda (num) (string->number (list->string num))))

  done)

  #;(new 
    (*parser (range #\1 #\9))
    (*parser (range #\0 #\9)) *star
    (*caten 2)
    (*pack-with
      (lambda (x xs)
        (string->number (list->string `(,x ,@xs))))
      )
    
    (*parser (char #\0))
    (*parser (range #\0 #\9))
    *not-followed-by
    (*pack (lambda (_) 0))


    (*disj 2)
  done)
)

(define <Integer>
  (new 
    (*parser (char #\+))
    (*parser <Natural>)
    (*caten 2)
    (*pack-with
      (lambda (plus nat) nat)
    )

    (*parser (char #\-))
    (*parser <Natural>)
    (*caten 2)
    (*pack-with
      (lambda (minus nat) (- nat))
    )

    (*parser <Natural>)
    (*disj 3)
  done)
)

(define <Fraction>
  (new 
    (*parser <Integer>)
    (*parser (char #\/))
    
    (*parser <Natural>)
    (*caten 3)

    (*pack-with 
      (lambda (int // nat)
        (/ int nat)
      )
    )
  done)
)

(define <Number>
  (new
    (*parser <Fraction>)
    (*parser <Integer>)
    (*disj 2)
  done)
)

(define <Arithmetics>
  (new
    (*parser (char #\^))
    (*parser (char #\*))
    (*parser (char #\-))
    (*parser (char #\+))
    (*parser (char #\/))
    (*disj 5)
  done)
)






(define <alphabetic>
  (const (lambda (ch) (and (char<=? #\a ch) (char<=? ch #\z)))))



;*******************************





(define <StringLiteralChar> 
  (new 
    (*parser <any>)

    (*parser (char #\\))  
    (*parser (char #\"))
    (*disj 2)
    *diff
  done)
)

(define <StringMetaChar>
  (new

    (*parser (word-ci "\\\\"))
    (*pack (lambda (_) #\\))

    (*parser (word-ci "\\\""))
    (*pack (lambda (_) #\"))

    (*parser (word-ci "\\t"))
    (*pack (lambda (_) #\tab))

    (*parser (word-ci "\\f"))
    (*pack (lambda (_) #\page))

    (*parser (word-ci "\\n"))
    (*pack (lambda (_) #\newline))

    (*parser (word-ci "\\r"))
    (*pack (lambda (_) #\return))

    (*disj 6)
  done)
)

(define <StringHexChar>
  (new
    (*parser (word-ci "\\x"))
    (*parser <HexChar>) *star
    (*parser (char #\;))
    (*caten 3)
    (*pack-with 
      (lambda (x hex semi)
        (integer->char
          (string->number 
            (list->string hex)
            16))
      )
    )
  done)
)

(define <StringChar>
  (new    
    (*parser <StringMetaChar>)
    (*parser <StringHexChar>)
    (*parser <StringLiteralChar>)
    
    (*disj 3)
  done)
)

(define <String>
  (new 
    (*parser (char #\"))
    (*parser <StringChar>) *star
    (*parser (char #\"))
    (*caten 3)
    (*pack-with
      (lambda (dq1 str dq2) (list->string str)))
  done)
)



(define <Quoted>
  (new 
    (*parser (char #\'))
    (*delayed (lambda () <sexpr>))
    (*caten 2)

    (*pack-with
      (lambda (_ sxp)
        `'(,@sxp)
      )
    )
  done)
)

(define <QuasiQuoted>
  (new 
    (*parser (char #\`))
    (*delayed (lambda () <sexpr>))
    (*caten 2)

    (*pack-with
      (lambda (_ sxp)
        (list 'quasiquote sxp)
      )
    )
  done)
)


(define <Unquoted>
  (new 
    (*parser (char #\,))
      (*delayed (lambda () <sexpr>))
      (*caten 2)

      (*pack-with
        (lambda (_ sxp)
          (list 'unquote sxp)
        )
      )
  done)
)

(define <UnquoteAndSpliced>
  (new
    (*parser (word ",@"))
      (*delayed (lambda () <sexpr>))
      (*caten 2)

      (*pack-with
        (lambda (_ sxp)
          (list 'unquote-splicing sxp)
        )
      )
  done)
)

(define <CBNameSyntax1>
  (new
    (*parser (char #\@))
      (*delayed (lambda () <sexpr>))
      (*caten 2)

      (*pack-with
        (lambda (_ sxp)
          (list 'cbname sxp)
        )
      )
  done)
)

(define <CBNameSyntax2>
  (new
    (*parser (char #\{))
      (*delayed (lambda () <sexpr>))
      (*parser (char #\}))
      (*caten 3)

      (*pack-with
        (lambda (rp sxp lp)
          (list 'cbname sxp)  
        )
      )
  done)
)

(define <CBName>
  (new
    (*parser <CBNameSyntax1>)
    (*parser <CBNameSyntax2>)
    (*disj 2)
  done)
)

(define <line_comment> 
  (new
    (*parser (char #\;))

    (*parser <any>)
    (*parser (char #\newline))
    (*parser <end-of-input>)
    (*disj 2)
    *diff
    *star

    (*parser (char #\newline))
    (*parser <end-of-input>)
    (*disj 2)

    (*caten 3)
  done)
)

(define <expr_comment> 
  (new
    (*parser (word "#;"))
    (*delayed (lambda () <sexpr>))
    (*caten 2)
  done)
)

(define <infix_expr_comment> 
  (new
    (*parser (word "#;"))
    (*delayed (lambda () <InfixExpression>))
    (*caten 2)
  done)
)

(define <comments-and-spaces>
  (new
    (*parser <line_comment>)
    (*parser <expr_comment>)
    (*parser <Whitespaces>)
    (*disj 3)
    ;*star
  done)
)

(define <comments>
  (new
    (*parser <line_comment>)
    (*parser <expr_comment>)
    (*disj 2)
    ;*star
  done)
)

(define <infix-comments-and-spaces>
	(new
		(*parser <line_comment>)
	    (*parser <infix_expr_comment>)
	    (*parser <Whitespaces>)
	    (*disj 3)
	done)
)

(define <infix-comments>
  (new
    (*parser <line_comment>)
    (*parser <infix_expr_comment>)
    (*disj 2)
    ;*star
  done)
)

(define <clean_comments>
  (lambda (<parser>)
     (new
      (*parser <comments-and-spaces>)
      *star
      (*parser <parser>)
      (*parser <comments-and-spaces>)
      *star
      (*caten 3)
      (*pack-with (lambda (first p second) p))
      done)
  ))

(define <infix-clean_comments>
  (lambda (<parser>)
     (new
      (*parser <infix-comments-and-spaces>)
      *star
      (*parser <parser>)
      (*parser <infix-comments-and-spaces>)
      *star
      (*caten 3)
      (*pack-with (lambda (first p second) p))
      done)
  ))

#;(define <infix-clean_comments>
  (lambda (<parser>)
     (new
      (*parser <infix-comments>)
      (*parser <parser>)
      (*parser <infix-comments>)
      (*caten 3)
      (*pack-with (lambda (first p second) p))
      done)
  ))
;********************************************************

(define <InfixPrefixExtensionPrefix>
  ;(<clean_spaces> (<infix-clean_comments>
    (new
      (*parser (word "##"))
      (*parser (word "#%"))
      (*disj 2)
    done);))
)

(define <InfixParen>
  ;(<infix-clean_comments>
    (new
      (*parser (char #\())
      (*delayed (lambda () <InfixExpression>))
      (*parser (char #\)))
      (*caten 3)
      (*pack-with
        (lambda (op infExp cp)
          infExp)
      )
    done);)
)

(define <InfixSymbolChar>
  ;(<infix-clean_comments>
    (new
      (*parser (range #\0 #\9))
      (*parser (range-ci #\a #\z))
      (*parser (char #\!))
      (*parser (char #\$))
      (*parser (char #\_))
      (*parser (char #\=))
      (*parser (char #\<))
      (*parser (char #\>))
      (*parser (char #\?))
        (*disj 9)
    done);)
)
	
(define <InfixSymbol>
  ;(<infix-clean_comments>
    (new
      (*parser <InfixSymbolChar>)
      *plus
      (*pack (lambda (sym)
          (string->symbol (string-downcase (list->string sym)))))
    done);)
)

(define <InfixNeg>
  ;(<infix-clean_comments>
    (new
      (*parser (char #\-))
      (*delayed (lambda () <Funcs>))
      (*caten 2)
      (*pack-with
        (lambda (_ infExp) `(-,infExp))
      )
    done);)
)



(define <InfixSexprEscape>
  ;(<infix-clean_comments>
    (new
      (*parser <InfixPrefixExtensionPrefix>)
      (*delayed (lambda () <sexpr>))
      (*caten 2)
      (*pack-with
        (lambda (_ sxpr) sxpr)
      )
    done);)
)

(define <InfixAtoms>
  (<infix-clean_comments>
    (new
      (*parser <InfixParen>)
      (*parser <Number>)
      (*parser <InfixSymbol>)
      *not-followed-by
      (*parser <InfixSymbol>)
      (*parser <InfixSexprEscape>)
      (*parser <InfixNeg>)
      (*disj 5)
    done))
)

(define <InfixArgList>
  ;(<infix-clean_comments>
    (new
      (*delayed (lambda () <InfixExpression>))

      (*parser (char #\,))
      (*delayed (lambda () <InfixExpression>))
      (*caten 2)
      (*pack-with (lambda (_ infexp) infexp))
      *star

      (*parser <epsilon>)
      (*disj 2)

      (*caten 2)
      (*pack-with (lambda (first rest) `(,first ,@rest)))

    done);)
)

(define <InfixFuncall>
  ;(<infix-clean_comments>
    (new
      (*parser (char #\())
      (*parser <InfixArgList>)
      (*parser (char #\)))
      (*caten 3)

      (*pack-with (lambda (op args cp) (lambda (f) (cons f args))))
    done);)
)

(define <InfixArrayGet>
  ;(<infix-clean_comments>
    (new
      (*parser (char #\[))
      (*delayed (lambda () <InfixExpression>))
      (*parser (char #\]))
      (*caten 3)

      (*pack-with (lambda (op infexp cp) (lambda (ag) `(vector-ref ,ag ,infexp))))
    done);)
)

(define <InfixArrGetAndFuncall>
  (<infix-clean_comments>
    (new

      (*parser <InfixFuncall>)
      (*parser <InfixArrayGet>)
      (*disj 2)
      *plus
    done))
)

(define <InfixArrayGetAndFuncall>
  ;(<infix-clean_comments>
    (new
      (*parser <InfixAtoms>)
      (*parser <InfixArrGetAndFuncall>)
      ;(*parser <InfixFuncall>)
      ;(*parser <InfixArrayGet>)
      ;(*disj 2)
      ;*plus

      (*caten 2)
      (*pack-with (lambda (exp args) (fold-left (lambda (x y) (y x)) exp args)))
    done);)
)





(define <Funcs>
  ;(<infix-clean_comments>
    (new
      
      (*parser <InfixArrayGetAndFuncall>)
      (*parser <InfixAtoms>)
      (*disj 2)
    done);)
)

(define <PowerSymbol>
  (new
    (*parser (char #\^))
    (*parser (word "**"))
    (*disj 2)
    ;(*pack (lambda (op) (string->symbol (string op))))
  done)
)

(define <InfixPow>
  ;(<infix-clean_comments>
    (new
      (*parser <Funcs>)

      (*parser <PowerSymbol>)
      (*parser <Funcs>)
      (*caten 2)
      *star

      (*caten 2)

      ; maybe try with fold-left

      
      (*pack-with (lambda (base pow) 
        (letrec ((inner (lambda (rest)
            (if (null? (cdr rest))
                (car rest)
                `(expt ,(car rest) ,(inner (cdr rest)))
                )
        )))
        (if (null? pow)
            base
            `(expt ,base ,(inner (map (lambda (elem) (cadr elem)) pow)))
        )
      )))
    done);)
)

(define <MulDivSymbol>
  (new
    (*parser (char #\*))
    (*parser (char #\/))
    (*disj 2)
    (*pack (lambda (op) (string->symbol (string op))))
  done)
)

(define <InfixMulDiv>
  ;(<infix-clean_comments>
    (new
      (*parser <InfixPow>)

      (*parser <MulDivSymbol>)
      (*parser <InfixPow>)
      (*caten 2)
      *star

      (*caten 2)


    (*pack-with (lambda (first second) 
        (letrec ((inner (lambda (exp rest)
          (if (null? rest)
              exp
              (inner `(,(caar rest) ,exp ,(cadar rest)) (cdr rest) )
        ))))

        (if (null? second)
            first
            (inner `(,(caar second) ,first ,(cadar second)) (cdr second))
        )
      )))
    done);)
)

(define <AddSubSymbol>
  (new
    (*parser (char #\+))
    (*parser (char #\-))
    (*disj 2)
    (*pack (lambda (op) (string->symbol (string op))))
  done)
)

(define <InfixAddSub>
  ;(<infix-clean_comments>
    (new
      (*parser <InfixMulDiv>)

      (*parser <AddSubSymbol>)
      (*parser <InfixMulDiv>)
      (*caten 2)
      *star

      (*caten 2)

      (*pack-with (lambda (first second) 
        (letrec ((inner (lambda (exp rest)
          (if (null? rest)
              exp
              (inner `(,(caar rest) ,exp ,(cadar rest)) (cdr rest) )
        ))))

        (if (null? second)
            first
            (inner `(,(caar second) ,first ,(cadar second)) (cdr second))
        )
      )))
  done);)
)



(define <InfixExpression>
  ;(<infix-clean_comments>
    (new
      (*parser <InfixAddSub>)
    done);)
)

(define <InfixExtension>
  (new
    (*parser <InfixPrefixExtensionPrefix>)
    (*delayed (lambda () <InfixExpression>))
    (*caten 2)
    (*pack-with
      (lambda (_ infexp) infexp)
    )
  done)
)


(define <sexpr>
  ;; fill in the s-expression parser details here
  (<clean_comments>
    (new
      (*parser <Boolean>)
      (*parser <Char>)

      (*parser <Number>)
      (*parser <Symbol>)
      *not-followed-by

      (*parser <String>)
      (*parser <Symbol>)
      *not-followed-by
      
      (*parser <Symbol>)
      (*parser <ProperList>)
      (*parser <ImproperList>)
      (*parser <Vector>)
      (*parser <Quoted>)
      (*parser <QuasiQuoted>)
      (*parser <UnquoteAndSpliced>)
      (*parser <Unquoted>)
      (*parser <CBName>)
      (*parser <InfixExtension>)

      (*disj 14)
      done)
    )
)