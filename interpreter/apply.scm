(define (interpreter-apply proc args ctx istail)
  (cond ((is-lambda? proc)
         (let ((apply-command (construct-apply-command proc args)))
           (if istail
               (ret/ctx ctx apply-command)
               (let loop ((v apply-command))
                 (let ((v (let/ctx ctx ctx
                            (let ((proc (get-apply-command-proc v)))
                              (call-with-new-frame
                                (get-lambda-env proc)
                                (lambda (env)
                                  (bind-parameter (get-lambda-parameter proc)
                                                  (get-apply-command-args v)
                                                  env)
                                  (interpreter-body (get-lambda-body proc) env ctx)))))))
                   (if (is-apply-command? v) (loop v) v))))))

        ((is-continuation? proc)
         (if (check-continuation-arity (get-continuation-arity proc)
                                       (length args))
             (begin
               (exec-dynamic-wind-proc (get-context-wind-tags
                                         (construct-context/cont proc))
                                       (get-context-wind-tags ctx))
               (apply (get-continuation-proc proc) args))
             (error "Continuation's arity not matched")))

        ((is-internal-function? proc)
         (apply (get-internal-function-proc proc) ctx istail args))

        (else
          (error "Cannot apply non-functional value" (vartype proc)))))

(define (exec-dynamic-wind-proc to-tags from-tags)
  (define (go-down-from-tags from-tags) ; {{{
    (cond
      ;; When from-tags is exhausted, go up the to-tags.
      ((null? from-tags) (go-up-to-tags (reverse to-tags) '()))

      ;; If to-tags includes from-tags, left to do is go up the to-tags.
      ((assoc (car (first from-tags)) to-tags)
       (let loop ((rev-to-tags (reverse to-tags)) (past-tags '()))
         (cond ((null? rev-to-tags) #t)
               ((eq? (car (first rev-to-tags)) (car (first from-tags)))
                (go-up-to-tags (cdr rev-to-tags) past-tags))
               (else (loop (cdr rev-to-tags)
                           (cons (first rev-to-tags) past-tags))))))
      ;; Otherwise we go down the from-tags.
      (else
        (let/ctx-tags from-tags ctx
          (interpreter-apply (get-context-wind-tag-after (first from-tags))
                             '() ctx #f))
        (go-down-from-tags (cdr from-tags))))) ; }}}
  (define (go-up-to-tags rev-to-tags past-tags) ; {{{
    ;; to-tags is reversed, so we just execute the before proc.
    (if (null? rev-to-tags) #t
        (begin
          (let/ctx-tags (cons (first rev-to-tags) past-tags) ctx
             (interpreter-apply (get-context-wind-tag-before (first rev-to-tags))
                                '() ctx #f))
          (go-up-to-tags (cdr rev-to-tags)
                         (cons (first rev-to-tags) past-tags))))) ; }}}
  (go-down-from-tags from-tags))

(define (bind-parameter parameters args env)
  (letrec ((F (lambda (p a)
                (cond ((and (null? p) (null? a)) #t)
                      ((symbol? p)
                       (add-identifier! p env)
                       (set-identifier! p (construct-list a) env)
                       #t)
                      ((and (pair? p) (pair? a))
                       (add-identifier! (car p) env)
                       (set-identifier! (car p) (car a) env)
                       (F (cdr p) (cdr a)))
                      (else
                        (error "Wrong number of arguments."))))))
          (F parameters args)))

(define (check-continuation-arity count argcount)
  (cond ((= count -1) #t)
        ((= count argcount) #t)
        (else (error "Wrong number of arguments." count argcount))))

