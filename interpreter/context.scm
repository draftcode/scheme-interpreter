(define get-context-wind-tag-after third)
(define get-context-wind-tag-before second)

(define (construct-context continuation wind-tags)
  (list continuation wind-tags))

(define (construct-context/cont val)
  (list (get-continuation-proc val)
        (get-continuation-wind-tags val)))

(define (get-context-proc ctx)
  (first ctx))

(define (get-context-wind-tags ctx)
  (second ctx))

(define (call/ctx-tags tags proc)
  (call/cc
    (lambda (cont)
      (proc (construct-context cont tags)))))

(define (call/ctx ctx proc)
  (call/ctx-tags
    (if (null? ctx) '() (get-context-wind-tags ctx))
    proc))

(define (call/ctx-dynwind ctx sym before after proc)
  (call/ctx-tags
    (cons (list sym before after)
          (if (null? ctx)
              '()
              (get-context-wind-tags ctx)))
    proc))

(define-syntax let/ctx-tags
  (syntax-rules ()
    ((_ T L rest ...)
     (call/ctx-tags T (lambda (L) rest ...)))))

(define-syntax let/ctx
  (syntax-rules ()
    ((_ C L rest ...)
     (call/ctx C (lambda (L) rest ...)))))

(define-syntax let/ctx-dynwind
  (syntax-rules ()
   ((_ C L sym before after rest ...)
    (call/ctx-dynwind C sym before after (lambda (L) rest ...)))))

(define (ret/ctx ctx v)
  ((get-context-proc ctx) v))

