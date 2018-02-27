"char->integer"
(char->integer #\A)
(char->integer #\Z)
(char->integer #\a)
(char->integer #\z)
(char->integer #\3)

;shouldn't work

#;(
(char->integer)
(char->integer a)
(char->integer 3)
(char->integer #\a #\a)
(char->integer "fs")
)

"integer->char"

(integer->char 3)
(integer->char 33)
(integer->char 0)
;(integer->char 127)
;(integer->char -1) ; #\ÿ
;(integer->char 256) ; #\Ā

#;(
(integer->char)
(integer->char #\A)
(integer->char a)
(integer->char 3)
(integer->char #\a #\a)
(integer->char "fs")
)