
(define (interpreter-macro-transformer expr env)
  (cond ((eq? (first expr) 'syntax-rules)
         (interpreter-syntax-rules (second expr) (cddr expr) env))
        (else
          (error "Unknown macro transformer" (first expr)))))

