
(define (interpreter-define-syntax name rest-expr env)
  (add-identifier! name env)
  (set-identifier! name (interpreter-macro-transformer rest-expr env) env))

