(define (interpreter-body body-expr env ctx)
  (let loop ((body-expr body-expr))
    (if (null? body-expr)
        (ret/ctx ctx (construct-undefined))
        (if (and (pair? (first body-expr))
                 (symbol? (first (first body-expr)))
                 (is-syntax-define? (find-in-environment (first (first body-expr)) env)))
            (begin
              (interpreter-define (first body-expr) env ctx)
              (loop (cdr body-expr)))
            (interpreter-body-expr body-expr env ctx)))))

(define (interpreter-body-expr body-expr env ctx)
  (let loop ((body-expr body-expr))
    (if (null? (cdr body-expr))
        (interpreter-expr-tail (first body-expr) env ctx)
        (begin
          (let/ctx ctx ctx (interpreter-expr-nottail (car body-expr) env ctx))
          (loop (cdr body-expr))))))

(define (is-syntax-define? obj)
  (and (is-syntax-object? obj) (eq? (get-syntax-name obj) 'define)))

