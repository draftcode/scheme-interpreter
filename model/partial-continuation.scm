(define resetcont #f)

(define-syntax reset
  (syntax-rules ()
    ((_ F ...)
     (let/cc c
             (set! resetcont c)
             (let ((r (begin F ...)))
               (resetcont r))))))

(define-syntax shift
  (syntax-rules ()
    ((_ kont F ...)
     (let/cc shiftcont
             (let ((k resetcont)
                   (kont (lambda (v)
                           (let/cc incont
                                   (set! resetcont incont)
                                   (shiftcont v)))))
                 (k (begin F ...)))))))

(define promptlist '())
(define (appendlist c) (set! promptlist
                             (append promptlist
                                     (list c))))
(define (poplist v)
  (let ((c (car promptlist)))
    (set! promptlist (cdr promptlist))
    (c v)))

(define-syntax prompt
  (syntax-rules ()
    ((_ F ...)
     (let/cc c
             (let ((r (begin F ...)))
               (appendlist c)
               (poplist r))))))

(define-syntax control
  (syntax-rules ()
    ((_ kont F ...)
     (let/cc controlcont
             (let ((kont (lambda (v)
                           (let/cc incont
                                   (appendlist incont)
                                   (controlcont v)))))
               (let ((r (begin F ...)))
                 (poplist r)))))))

(display (prompt
           (for-each (lambda (x)
                       (control k (cons x (k #f))))
                     '(1 2 3))
           '()))

;; (display (reset
;;            (for-each (lambda (x)
;;                        (shift k (cons x (k #f))))
;;                      '(1 2 3))
;;            '()))

;; (define (generate iterate collection)
;;   (shift yield
;;          (iterate yield collection)))
;; 
;; (reset
;;   (display (generate for-each '(1 2 3)))
;;   (newline))

