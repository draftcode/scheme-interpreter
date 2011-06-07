(define c undefined)

(dynamic-wind
  (lambda () (print "before"))
  (lambda () (dynamic-wind (lambda () (print "before-2"))
                           (lambda () (call/cc (lambda (cont) (set! c cont) 1)))
                           (lambda () (print "after-2"))))
  (lambda () (print "after")))

(dynamic-wind
  (lambda () (print "before-3"))
  (lambda () (dynamic-wind (lambda () (print "before-4"))
                           (lambda () (c 2))
                           (lambda () (print "after-4"))))
  (lambda () (print "after-3")))

(c 2)

