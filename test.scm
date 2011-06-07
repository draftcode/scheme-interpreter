(define c #f)

(reset 
  (dynamic-wind
    (lambda () (print "before"))
    (lambda () (dynamic-wind (lambda () (print "before-2"))
                             (lambda () (shift cont (set! c cont) 1))
                             (lambda () (print "after-2"))))
    (lambda () (print "after"))))

(c 2)

