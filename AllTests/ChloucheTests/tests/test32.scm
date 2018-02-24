 ((lambda (a . d) d) 1 2 3 4 6 7 8 9 10)      ; packing the optional arguments to a list - should return (4 . (6 . (7 . (8 . ()))))
