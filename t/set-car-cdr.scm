(define p (cons 'a (cons 1 2)))

p ; (a 1 . 2)

(set-car! (cdr p) 3)

p ; (a 3 . 2)

(set-cdr! (cdr p) 4)

p ; (a 3 . 4)

