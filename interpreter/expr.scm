;; interpreter-expr {{{
(define (interpreter-expr expr env ctx istail)
  (define (interpreter-expr-atom expr)
    (cond
      ((boolean? expr) (ret/ctx ctx (construct-boolean expr)))
      ((number? expr)  (ret/ctx ctx (construct-number expr)))
      ((char? expr)    (ret/ctx ctx (construct-char expr)))
      ((string? expr)  (ret/ctx ctx (construct-string expr)))
      ((symbol? expr)
       (let ((r (find-in-environment expr env)))
         (if r (ret/ctx ctx r)
             (error "Unbound variable." (find-original-name expr env)))))
      (else  (error "Unknown expression."))))
  (define (interpreter-expr-pair first-obj rest-expr)
    (cond
      ((is-syntax-quote? first-obj)         (interpreter-expr-quote rest-expr env ctx istail))
      ((is-syntax-lambda? first-obj)        (interpreter-expr-lambda rest-expr env ctx istail))
      ((is-syntax-if? first-obj)            (interpreter-expr-if rest-expr env ctx istail))
      ((is-syntax-set!? first-obj)          (interpreter-expr-set! rest-expr env ctx istail))
      ((is-syntax-let-syntax? first-obj)    (error "Not implemented."))
      ((is-syntax-letrec-syntax? first-obj) (error "Not implemented."))
      ((is-macro-transformer? first-obj)    (interpreter-expr-macro-expand first-obj rest-expr env ctx istail))
      ;; derived expression
      ;; TODO 
      ((is-function? first-obj)             (interpreter-expr-apply first-obj rest-expr env ctx istail))
      (else                                 (error "Invalid application." first-obj))))

  (if (pair? expr)
      (interpreter-expr-pair
        (let/ctx ctx ctx (interpreter-expr-nottail (car expr) env ctx))
        (cdr expr))
      (interpreter-expr-atom expr)))
;; }}}

(define (interpreter-expr-tail expr env ctx)
  (interpreter-expr expr env ctx #t))

(define (interpreter-expr-nottail expr env ctx)
  (interpreter-expr expr env ctx #f))

(define (is-syntax-quote? obj)         (and (is-syntax-object? obj) (eq? (get-syntax-name obj) 'quote        )))
(define (is-syntax-lambda? obj)        (and (is-syntax-object? obj) (eq? (get-syntax-name obj) 'lambda       )))
(define (is-syntax-if? obj)            (and (is-syntax-object? obj) (eq? (get-syntax-name obj) 'if           )))
(define (is-syntax-set!? obj)          (and (is-syntax-object? obj) (eq? (get-syntax-name obj) 'set!         )))
(define (is-syntax-let-syntax? obj)    (and (is-syntax-object? obj) (eq? (get-syntax-name obj) 'let-syntax   )))
(define (is-syntax-letrec-syntax? obj) (and (is-syntax-object? obj) (eq? (get-syntax-name obj) 'letrec-syntax)))

(define (is-function? obj)
  (or (is-lambda? obj)
      (is-continuation? obj)
      (is-internal-function? obj)))

;; interpreter-expr-quote {{{
(define (interpreter-expr-quote rest-expr env ctx istail)
  (ret/ctx ctx
    (interpreter-quote
      (car (let loop ((expr rest-expr))
             (cond
               ((pair? expr) (cons (loop (car expr)) (loop (cdr expr))))
               ((symbol? expr) (get-true-ident expr env))
               (else expr)))))))
;; }}}
;; interpreter-expr-lambda {{{
(define (interpreter-expr-lambda rest-expr env ctx istail)
  (ret/ctx ctx (construct-lambda (first rest-expr) (cdr rest-expr) env)))
;; }}}
;; interpreter-expr-if {{{
(define (interpreter-expr-if rest-expr env ctx istail)
  (interpreter-if (first rest-expr) (second rest-expr)
                  (if (= (length rest-expr) 3) (third rest-expr) '())
                  env ctx istail))
;; }}}
;; interpreter-expr-set! {{{
(define (interpreter-expr-set! rest-expr env ctx istail)
  (if (not (symbol? (first rest-expr))) (error "Variable name is not symbol."))
  (set-identifier! (get-true-ident (first rest-expr) env)
                   (let/ctx ctx ctx
                            (interpreter-expr-nottail (second rest-expr) env ctx))
                   env)
  (ret/ctx ctx (construct-undefined)))
;; }}}
;; interpreter-expr-apply {{{
(define (interpreter-expr-apply proc args-expr env ctx istail)
  (let ((args (map (lambda (expr)
                     (let/ctx ctx ctx
                              (interpreter-expr-nottail expr env ctx)))
                   args-expr)))
    (interpreter-apply proc args ctx istail)))
;; }}}
;; interpreter-expr-macro-expand {{{
(define (interpreter-expr-macro-expand transformer usage env ctx istail)
  (interpreter-expr ((get-macro-transformer-proc transformer) usage env)
                    env
                    ctx
                    istail))
;; }}}

;; get-true-ident ... リファレンスの真の名前を探す． {{{
(define (get-true-ident ident env)
  (let ((val (get-identifier ident env)))
    (if (or (not val) (not (is-reference? val))) ident
        (get-true-ident (get-reference-ident val) (get-reference-env val)))))
;; }}}

