
(define c #f)

(reset (+ 1 2 (shift k (set! c k) 3))) ; => 3

(c 3) ; => 6

