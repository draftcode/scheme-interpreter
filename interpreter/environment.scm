(define (empty-environment) (cons '() '()))
(define (empty-environment? env) (null? env))

(define (append-new-frame env) (cons env '()))
(define (upper-frame env) (car env))
(define (call-with-new-frame env func)
  (func (append-new-frame env)))

(define (get-top-frame env) (cdr env))
(define (set-top-frame! frame env) (set-cdr! env frame))

(define (enumerate-frames f default env)
  (let loop ((env env))
    (if (empty-environment? env) default
        (let ((r (f (get-top-frame env))))
          (if r r (loop (upper-frame env)))))))

(define (find-reference-binding ident env)
  (let loop ((frame (get-top-frame env)))
    (cond ((null? frame) #f)
          ((and (is-reference? (cdar frame))
                (eq? ident (get-reference-ident (cdar frame))))
           (car frame))
          (else (loop (cdr frame))))))

;; get-identifier {{{
(define (get-identifier-in-frame ident frame)
  (assq ident frame))

(define (get-identifier ident env)
  (cdr (enumerate-frames (lambda (frame) (get-identifier-in-frame ident frame))
                         (cons #f #f) env)))
;; }}}
;; add-identifier! {{{
(define (add-identifier! ident env)
  (if (not (get-identifier-in-frame ident (get-top-frame env)))
      (set-top-frame! (cons (cons ident (construct-undefined))
                            (get-top-frame env))
                      env))
  #t)
;; }}}
;; set-identifier! {{{
(define (set-identifier! ident val env)
  (enumerate-frames (lambda (frame)
                      (let ((pair (get-identifier-in-frame ident frame)))
                        (if pair (begin (set-cdr! pair val) pair #t) #f)))
                    #f
                    env))
;; }}}

;; get-identifier-defined-env {{{
(define (get-identifier-defined-env ident env)
  (let loop ((env env))
    (if (empty-environment? env) #f
        (if (get-identifier-in-frame ident (get-top-frame env))
            env
            (loop (upper-frame env))))))
;; }}}

