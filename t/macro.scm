(define-syntax test
  (syntax-rules ()
    ((_ A ...) (f A ...))))

(define (f x y)
  (* x y))

(define (g)
  (define (f x y)
    (+ x y))
  (test 1 2))

(g)

