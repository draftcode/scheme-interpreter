(define (interpreter-if q-expr t-expr f-expr env ctx istail)
  (let ((q (let/ctx ctx ctx
             (interpreter-expr-nottail q-expr env ctx))))
    (if (and (is-boolean? q) (not (get-boolean-value q)))
        (if (null? f-expr)
            (construct-undefined)
            (interpreter-expr f-expr env ctx istail))
        (interpreter-expr t-expr env ctx istail))))

