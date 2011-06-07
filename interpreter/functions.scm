
(define-syntax define-valiable ; {{{
  (syntax-rules ()
    ((_ env name body ...)
     (begin (add-identifier! (quote name) env)
            (set-identifier! (quote name)
                             (begin body ...)
                             env))))) ; }}}
(define-syntax define-internal-function ; {{{
  (syntax-rules ()
    ((_ env (proc . args) body ...)
     (begin 
       (add-identifier! (quote proc) env)
       (set-identifier! (quote proc)
                        (construct-internal-function
                          (lambda (ctx istail . args) body ...))
                        env)))
    ((_ env ctx istail (proc args ...) body ...)
     (begin
       (add-identifier! (quote proc) env)
       (set-identifier! (quote proc)
                        (construct-internal-function
                          (lambda (ctx istail args ...) body ...))
                        env))))) ; }}}
(define-syntax define-internal-macro ; {{{
  (syntax-rules (syntax-rules)
    ((_ macroenv name (syntax-rules literal (pattern templates) ...))
     (begin
       (add-identifier! (quote name) macroenv)
       (set-identifier! (quote name)
                        (interpreter-syntax-rules
                          'literal '((pattern templates) ...) macroenv)
                        macroenv))))) ; }}}
(define (check proc errmsg l) ; {{{
  (map (lambda (v) (if (not (proc v)) (error errmsg))) l)) ; }}}

(define (default-environment)
  (let ((env (empty-environment)))

    (define-valiable env quote         (construct-syntax-object 'quote))
    (define-valiable env lambda        (construct-syntax-object 'lambda))
    (define-valiable env if            (construct-syntax-object 'if))
    (define-valiable env set!          (construct-syntax-object 'set!))
    (define-valiable env let-syntax    (construct-syntax-object 'let-syntax))
    (define-valiable env letrec-syntax (construct-syntax-object 'letrec-syntax))
    (define-valiable env define        (construct-syntax-object 'define))
    (define-valiable env undefined     (construct-undefined))

    ;; TODO: 現在はタグ付きデータを直接受け取ってそれをいじるようにしているが，
    ;; 直接インタプリタの関数にした方が書きやすいものもある．(書けないものもあ
    ;; る)

    ;; Numbers {{{
    (define-internal-function env (number? v1) ; {{{
      (construct-boolean (is-number? v1))) ; }}}
    (define-internal-function env (= v1 v2) ; {{{
      (if (or (not (is-number? v1)) (not (is-number? v2)))
          (error "Value is not number.")
          (construct-boolean (= (get-number-value v1)
                                (get-number-value v2))))) ; }}}
    (define-internal-function env (< v1 v2) ; {{{
      (if (or (not (is-number? v1)) (not (is-number? v2)))
          (error "Value is not number.")
          (construct-boolean (< (get-number-value v1)
                                (get-number-value v2))))) ; }}}
    (define-internal-function env (<= v1 v2) ; {{{
      (if (or (not (is-number? v1)) (not (is-number? v2)))
          (error "Value is not number.")
          (construct-boolean (<= (get-number-value v1)
                                 (get-number-value v2))))) ; }}}
    (define-internal-function env (> v1 v2) ; {{{
      (if (or (not (is-number? v1)) (not (is-number? v2)))
          (error "Value is not number.")
          (construct-boolean (> (get-number-value v1)
                                (get-number-value v2))))) ; }}}
    (define-internal-function env (>= v1 v2) ; {{{
      (if (or (not (is-number? v1)) (not (is-number? v2)))
          (error "Value is not number.")
          (construct-boolean (>= (get-number-value v1)
                                 (get-number-value v2))))) ; }}}
    (define-internal-function env (+ . l) ; {{{
      (check is-number? "Value is not number." l)
      (construct-number (fold-right (lambda (v r) (+ r (get-number-value v)))
                              0 l))) ; }}}
    (define-internal-function env (* . l) ; {{{
      (check is-number? "Value is not number." l)
      (construct-number (fold-right (lambda (v r) (* r (get-number-value v)))
                              1 l))) ; }}}
    (define-internal-function env (- . l) ; {{{
      (check is-number? "Value is not number." l)
      (construct-number (fold-right (lambda (v r) (- r (get-number-value v)))
                              (get-number-value (car l))
                              (cdr l)))) ; }}}
    (define-internal-function env (/ . l) ; {{{
      (check is-number? "Value is not number." l)
      (construct-number (fold-right (lambda (v r) (/ r (get-number-value v)))
                              (get-number-value (car l))
                              (cdr l)))) ; }}}
    (define-internal-function env (number->string v . option) ; {{{
      (cond ((> (length option) 1) (error "Too much arguments."))
            ((not (is-number? v)) (error "Value is not number."))
            ((and (= (length option) 1) (not (is-number? (car option))))
             (error "Value is not number."))
            (construct-string (apply number->string
                                     (get-number-value v)
                                     (map get-number-value option))))) ; }}}
    ;; }}}
    ;; Pairs {{{
    (define-internal-function env (null? v1) ; {{{
      (construct-boolean (is-null? v1))) ; }}}
    (define-internal-function env (pair? v1) ; {{{
      (construct-boolean (is-pair? v1))) ; }}}
    (define-internal-function env (symbol? v1) ; {{{
      (construct-boolean (is-symbol? v1))) ; }}}
    (define-internal-function env (list? v1) ; {{{
       (let loop ((v v1))
         (cond ((is-null? v) (construct-boolean #t))
               ((is-pair? v) (loop (get-pair-cdr v)))
               (else         (construct-boolean #f))))) ; }}}
    (define-internal-function env (length v1) ; {{{
      (construct-number
        (let loop ((v v1))
          (cond ((is-null? v) 0)
                ((is-pair? v) (+ 1 (loop (get-pair-cdr v))))
                (else         (error "Value is not list")))))) ; }}}
    (define-internal-function env (car v1) ; {{{
      (if (not (is-pair? v1))
          (error "Value is not pair")
          (get-pair-car v1))) ; }}}
    (define-internal-function env (cdr v1) ; {{{
      (if (not (is-pair? v1))
          (error "Value is not pair")
          (get-pair-cdr v1))) ; }}}
    (define-internal-function env (set-car! v1 v2) ; {{{
      (if (not (is-pair? v1))
          (error "Value is not pair")
          (set-pair-car! v1 v2))) ; }}}
    (define-internal-function env (set-cdr! v1 v2) ; {{{
      (if (not (is-pair? v1))
          (error "Value is not pair")
          (set-pair-cdr! v1 v2))) ; }}}
    (define-internal-function env (cons v1 v2) ; {{{
      (construct-pair v1 v2)) ; }}}
    (define-internal-function env (list . l) ; {{{
      (if (null? l) (construct-null) (construct-list l))) ; }}}
    (define-internal-function env (memq val l) ; {{{
       (let loop ((l l))
         (cond ((is-null? l) (construct-boolean #f))
               ((is-pair? l)
                (if (interpreter-eq? (get-pair-car l) val) l
                    (loop (get-pair-cdr l))))
               (else (error "Value is not list."))))) ; }}}
    (define-internal-function env (last l) ; {{{
       (let loop ((l l))
         (cond ((is-null? l) (error "Value is not pair."))
               ((is-pair? l)
                (if (is-null? (get-pair-cdr l)) l
                    (loop (get-pair-cdr l))))
               (else (error "Value is not list."))))) ; }}}
    (define-internal-function env (append l1 l2first . l2rest) ; {{{
       (let loop ((l1 l1) (l2 (cons l2first l2rest)))
         (cond ((is-null? l1)
                (if (null? (cdr l2)) (car l2) (loop (car l2) (cdr l2))))
               ((is-pair? l1)
                (construct-pair (get-pair-car l1)
                                (loop (get-pair-cdr l1) l2)))
               (else (error "Value is not list."))))) ; }}}
    ;; }}}
    ;; Booleans {{{
    (define-internal-function env (boolean? v1) ; {{{
      (construct-boolean (is-boolean? v1))) ; }}}
    (define-internal-function env (not v1) ; {{{
      (if (and (is-boolean? v1) (not (get-boolean-value v1)))
          (construct-boolean #t)
          (construct-boolean #f))) ; }}}
    ;; }}}
    ;; Strings {{{
    (define-internal-function env (string? v1) ; {{{
      (construct-boolean (is-string? v1))) ; }}}
    (define-internal-function env (string-append . l) ; {{{
      (check is-string? "Value is not string" l)
      (construct-string
        (apply string-append (map get-string-value l)))) ; }}}
    (define-internal-function env (symbol->string v) ; {{{
      (if (not (is-symbol? v)) (error "Value is not symbol.")
          (construct-string (symbol->string (get-symbol-value v))))) ; }}}
    (define-internal-function env (string->symbol v) ; {{{
      (if (not (is-string? v)) (error "Value is not string.")
          (construct-symbol (string->symbol (get-string-value v))))) ; }}}
    (define-internal-function env (string->number v . option) ; {{{
      (cond ((> (length option) 1) (error "Too much arguments."))
            ((not (is-string? v)) (error "Value is not string."))
            ((and (= (length option) 1) (not (is-number? (car option))))
             (error "Value is not number."))
            (construct-number (apply string->number
                                     (get-string-value v)
                                     (map get-number-value option))))) ; }}}
    ;; }}}
    ;; Procedures {{{
    (define-internal-function env (procedure? v1) ; {{{
      (construct-boolean (is-applicable? v1))) ; }}}
    ;; }}}
    ;; Equality {{{
    (define-internal-function env (eq? v1 v2) ; {{{
      (construct-boolean (interpreter-eq? v1 v2))) ; }}}
    (define-internal-function env (neq? v1 v2) ; {{{
      (construct-boolean (not (interpreter-eq? v1 v2)))) ; }}}
    (define-internal-function env (equal? v1 v2) ; {{{
      (construct-boolean (interpreter-equal? v1 v2))) ; }}}
    ;; }}}

    (define-internal-function env ctx istail (call/cc proc) ; {{{
                              (interpreter-apply proc (list (construct-continuation/ctx ctx))
                                                 ctx istail)) ; }}}
    (define-internal-function env ctx istail (dynamic-wind before thunk after) ; {{{
      (interpreter-apply before '() ctx #f)
      (let ((r (let/ctx-dynwind ctx ctx (gensym) before after
                               (interpreter-apply thunk '() ctx #f))))
        (interpreter-apply after '() ctx #f)
        r)) ; }}}
    (define-internal-macro env let/cc ; {{{
      (syntax-rules ()
        ((_ k body ...) (call/cc (lambda (k) body ...))))) ; }}}

    (define-valiable env resetcont (construct-undefined))
    (define-internal-macro env reset ; {{{
      (syntax-rules ()
        ((_ F ...)
         (let/cc c
                 (set! resetcont c)
                 (let ((r (begin F ...)))
                   (resetcont r)))))) ; }}}
    (define-internal-macro env shift ; {{{
      (syntax-rules ()
        ((_ kont F ...)
         (let/cc shiftcont
                 (let ((k resetcont)
                       (kont (lambda (v)
                               (let/cc incont
                                       (set! resetcont incont)
                                       (shiftcont v)))))
                   (k (begin F ...))))))) ; }}}

    (define-internal-function env (print . l) ; {{{
      (apply print l)
      (construct-undefined)) ; }}}

    ;; Derived expressions {{{
    (define-internal-macro env cond ; {{{
      (syntax-rules (else =>)
        ((cond (else result1 result2 ...))
         (begin result1 result2 ...))
        ((cond (test => result))
         (let ((temp test))
           (if temp (result temp))))
        ((cond (test => result) clause1 clause2 ...)
         (let ((temp test))
           (if temp
               (result temp)
               (cond clause1 clause2 ...))))
        ((cond (test)) test)
        ((cond (test) clause1 clause2 ...)
         (let ((temp test))
           (if temp
               temp
               (cond clause1 clause2 ...))))
        ((cond (test result1 result2 ...))
         (if test (begin result1 result2 ...)))
        ((cond (test result1 result2 ...)
               clause1 clause2 ...)
         (if test
             (begin result1 result2 ...)
             (cond clause1 clause2 ...))))) ; }}}
    (define-internal-macro env case ; {{{
      (syntax-rules (else)
        ((case (key ...) clauses ...)
         (let ((atom-key (key ...)))
           (case atom-key clauses ...)))
        ((case key (else result1 result2 ...))
         (begin result1 result2 ...))
        ((case key ((atoms ...) result1 result2 ...))
         (if (memv key '(atoms ...))
             (begin result1 result2 ...)))
        ((case key ((atoms ...) result1 result2 ...)
           clause clauses ...)
         (if (memv key '(atoms ...))
             (begin result1 result2 ...)
             (case key clause clauses ...))))) ; }}}
    (define-internal-macro env and ; {{{
      (syntax-rules ()
        ((and) #t)
        ((and test) test)
        ((and test1 test2 ...)
         (if test1 (and test2 ...) #f)))) ; }}}
    (define-internal-macro env or ; {{{
      (syntax-rules ()
        ((or) #f)
        ((or test) test)
        ((or test1 test2 ...)
         (let ((x test1))
           (if x x (or test2 ...)))))) ; }}}
    (define-internal-macro env let ; {{{
      (syntax-rules ()
        ((let ((name val) ...) body1 body2 ...)
         ((lambda (name ...) body1 body2 ...)
          val ...))
        ((let tag ((name val) ...) body1 body2 ...)
         ((letrec ((tag (lambda (name ...)
                          body1 body2 ...)))
            tag)
          val ...)))) ; }}}
    (define-internal-macro env let* ; {{{
      (syntax-rules ()
        ((let* () body1 body2 ...)
         (let () body1 body2 ...))
        ((let * ((name1 val1) (name2 val2) ...)
           body1 body2 ..)
         (let ((name1 val1))
           (let* ((name2 val2) ...)
             body1 body2 ...))))) ; }}}
    (define-internal-macro env letrec ; {{{
      (syntax-rules ()
        ((letrec ((var1 init1) ...) body ...)
         (letrec "generate_temp_names"
           (var1 ...)
           ()
           ((var1 init1) ...)
           body ...))
        ((letrec "generate_temp_names"
           ()
           (temp1 ...)
           ((var1 init1) ...)
           body ...)
         (let ((var1 undefined) ...)
           (let ((temp1 init1) ...)
             (set! var1 temp1)
             ...
             body ...)))
        ((letrec "generate_temp_names"
           (x y ...)
           (temp ...)
           ((var1 init1) ...)
           body ...)
         (letrec "generate_temp_names"
           (y ...)
           (newtemp temp ...)
           ((var1 init1) ...)
           body ...)))) ; }}}
    (define-internal-macro env begin ; {{{
      (syntax-rules ()
        ((begin exp ...)
         ((lambda () exp ...))))) ; }}}
    (define-internal-macro env do ; {{{
      (syntax-rules ()
        ((do ((var init step ...) ...)
             (test expr ...)
             command ...)
         (letrec
           ((loop
              (lambda (var ...)
                (if test
                    (begin
                      undefined
                      expr ...)
                    (begin
                      command ...
                      (loop (do "step" var step ...)
                            ...))))))
           (loop init ...)))
        ((do "step" x)
         x)
        ((do "step" x y)
         y))) ; }}}
    ;; }}}

    (append-new-frame env)))

