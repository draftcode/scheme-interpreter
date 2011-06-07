;; syntax-object {{{
(define (construct-syntax-object name)
  (list 'syntax name))

(define (is-syntax-object? val)
  (eq? (first val) 'syntax))

(define (get-syntax-name val)
  (if (is-syntax-object? val)
      (second val)
      (error "Value is not syntax-object.")))
;; }}}
;; undefined {{{
(define (construct-undefined)
  (list 'undef 'undef))

(define (is-undefined? val)
  (eq? (first val) 'undef))
;; }}}
;; boolean {{{
(define (construct-boolean expr)
  (list 'boolean expr))

(define (is-boolean? val)
  (eq? (first val) 'boolean))

(define (get-boolean-value val)
  (if (is-boolean? val)
      (second val)
      (error "Value is not boolean.")))
;; }}}
;; symbol {{{
(define (construct-symbol expr)
  (list 'symbol expr))

(define (is-symbol? val)
  (eq? (first val) 'symbol))

(define (get-symbol-value val)
  (if (is-symbol? val)
      (second val)
      (error "Value is not symbol.")))
;; }}}
;; char {{{
(define (construct-char expr)
  (list 'char expr))

(define (is-char? val)
  (eq? (first val) 'char))

(define (get-char-value val)
  (if (is-char? val)
      (second val)
      (error "Value is not char.")))
;; }}}
;; vector {{{
(define (construct-vector expr)
  (list 'vector expr))

(define (is-vector? val)
  (eq? (first val) 'vector))

(define (get-vector-value val)
  (if (is-vector? val)
      (second val)
      (error "Value is not vector.")))
;; }}}
;; number {{{
(define (construct-number expr)
  (list 'number expr))

(define (is-number? val)
  (eq? (first val) 'number))

(define (get-number-value val)
  (if (is-number? val)
      (second val)
      (error "Value is not number.")))
;; }}}
;; string {{{
(define (construct-string expr)
  (list 'string expr))

(define (is-string? val)
  (eq? (first val) 'string))

(define (get-string-value val)
  (if (is-string? val)
      (second val)
      (error "Value is not string.")))
;; }}}
;; pair (list)  {{{
(define (construct-pair car-value cdr-value)
  (list 'pair car-value cdr-value))

(define (construct-null)
  (list 'null))

(define (is-pair? val)
  (eq? (first val) 'pair))

(define (is-null? val)
  (eq? (first val) 'null))

(define (get-pair-car val)
  (if (is-pair? val) 
      (second val)
      (error "Value is not pair.")))

(define (get-pair-cdr val)
  (if (is-pair? val) 
      (third val)
      (error "Value is not pair.")))

(define (set-pair-car! val val2)
  (if (is-pair? val) 
      (set-car! (cdr val) val2)
      (error "Value is not pair.")))

(define (set-pair-cdr! val val2)
  (if (is-pair? val) 
      (set-car! (cddr val) val2)
      (error "Value is not pair.")))

(define (construct-list l-value)
  (if (null? l-value) (construct-null)
      (construct-pair (car l-value)
                      (construct-list (cdr l-value)))))
;; }}}

;; lambda {{{
(define (construct-lambda parameter body env)
  (list 'lambda env parameter body))

(define (is-lambda? val)
  (eq? (first val) 'lambda))

(define (get-lambda-parameter val)
  (if (is-lambda? val) 
      (third val)
      (error "Value is not lambda.")))

(define (get-lambda-body val)
  (if (is-lambda? val) 
      (fourth val)
      (error "Value is not lambda.")))

(define (get-lambda-env val)
  (if (is-lambda? val) 
      (second val)
      (error "Value is not lambda.")))
;; }}}
;; continuation {{{
(define (construct-continuation arity proc tags)
  (list 'continuation arity proc tags))

(define (construct-continuation/ctx ctx)
  (list 'continuation 1 (get-context-proc ctx) (get-context-wind-tags ctx)))

(define (is-continuation? val)
  (eq? (first val) 'continuation))

(define (get-continuation-arity val)
  (if (is-continuation? val)
      (second val)
      (error "Value is not continuation.")))

(define (get-continuation-proc val)
  (if (is-continuation? val)
      (third val)
      (error "Value is not continuation.")))

(define (get-continuation-wind-tags val)
  (if (is-continuation? val)
      (fourth val)
      (error "Value is not continuation.")))
;; }}}
;; internal-function {{{
(define (construct-internal-function proc)
  (list 'internal-function proc))

(define (is-internal-function? val)
  (eq? (first val) 'internal-function))

(define (get-internal-function-proc val)
  (if (is-internal-function? val)
      (second val)
      (error "Value is not internal function")))
;; }}}
;; macro-transformer {{{
(define (construct-macro-transformer proc)
  (list 'macro-transformer proc))

(define (is-macro-transformer? val)
  (eq? (first val) 'macro-transformer))

(define (get-macro-transformer-proc val)
  (if (is-macro-transformer? val)
      (second val)
      (error "Value is not macro transformer.")))
;; }}}
;; reference {{{
(define (construct-reference ident env)
  (list 'reference ident env))

(define (is-reference? val)
  (eq? (first val) 'reference))

(define (get-reference-ident val)
  (if (is-reference? val)
      (second val)
      (error "Value is not reference.")))

(define (get-reference-env val)
  (if (is-reference? val)
      (third val)
      (error "Value is not reference.")))
;; }}}
;; apply-command {{{
(define (construct-apply-command proc args)
  (list 'apply-command proc args))

(define (is-apply-command? val)
  (eq? (first val) 'apply-command))

(define (get-apply-command-proc val)
  (if (is-apply-command? val)
      (second val)
      (error "Value is not apply-command.")))

(define (get-apply-command-args val)
  (if (is-apply-command? val)
      (third val)
      (error "Value is not apply-command.")))
;; }}}

(define (is-applicable? v) ; {{{
  (or (is-lambda? v)
      (is-internal-function? v)
      (is-continuation? v))) ; }}}

(define (interpreter-eq? val1 val2) ; {{{
  (interpreter-equivalent? eq? val1 val2)) ; }}}
(define (interpreter-equal? val1 val2) ; {{{
  (interpreter-equivalent? equal? val1 val2)) ; }}}
(define (interpreter-equivalent? eq-proc val1 val2) ; {{{
  (let ((val1 (get-referenced-value val1))
        (val2 (get-referenced-value val2)))
    (define (both? pred) (and (pred val1) (pred val2)))
    (define (comp f) (eq-proc (f val1) (f val2)))
    (if (and val1 val2)
        (cond
          ((both? is-syntax-object?)     (comp get-syntax-name))
          ((both? is-undefined?)         #t)
          ((both? is-boolean?)           (comp get-boolean-value))
          ((both? is-symbol?)            (comp get-symbol-value))
          ((both? is-symbol?)            (comp get-symbol-value))
          ((both? is-char?)              (comp get-char-value))
          ((both? is-vector?)            (comp get-vector-value))
          ((both? is-number?)            (comp get-number-value))
          ((both? is-string?)            (comp get-string-value))
          ((both? is-pair?)              (eq-proc val1 val2))
          ((both? is-lambda?)            (eq-proc val1 val2))
          ((both? is-continuation?)      (eq-proc val1 val2))
          ((both? is-internal-function?) (eq-proc val1 val2))
          ((both? is-macro-transformer?) (eq-proc val1 val2))
          (else #f))
        #f))) ; }}}

