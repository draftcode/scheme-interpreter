
(define (odd? n)
  (if (= n 1) #t (even? (- n 1))))

(define (even? n)
  (if (= n 1) #f (odd? (- n 1))))

(even? -1)

