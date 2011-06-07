(define (interpreter-define form env ctx)
  (if (symbol? (second form))
      ;; Variable definision
      (let ((name (second form))
            (body (caddr form)))
        (add-identifier! name env)
        (set-identifier! name
                         (let/ctx ctx ctx
                                  (interpreter-expr-nottail body env ctx))
                         env))
      ;; Function definision
      (let ((name (first (second form)))
            (parameters (cdr (second form)))
            (body (cddr form)))
        (add-identifier! name env)
        (set-identifier! name
                         (construct-lambda parameters body env)
                         env))))

