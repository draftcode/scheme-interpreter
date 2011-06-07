
;; interpreter-syntax-rules {{{
(define (interpreter-syntax-rules literal templates macroenv)
  (let ((templates
          (map (lambda (template)
                 (cons (cdar template) (cdr template)))
               templates)))
    (construct-macro-transformer
      (lambda (expr env)
        (interpreter-syntax-rules-expand-macro expr
                                               env
                                               literal
                                               templates
                                               macroenv)))))
;; }}}
;; matched-pair {{{
(define (construct-matched-pair patval expr)
  (list 'matched-pair patval expr))

(define (construct-ellipsis-pairs patval pairs)
  (list 'ellipsis-pairs patval pairs))

(define (ellipsis-pairs? val)
  (eq? (first val) 'ellipsis-pairs))

(define (get-matched-pair-patval val) (second val))
(define (get-matched-pair-expr val)   (third  val))
(define (get-ellipsis-patval val)     (second val))
(define (get-ellipsis-pairs val)      (third  val))
(define (get-ellipsis-first-pair val)
  (let ((pairs (get-ellipsis-pairs val)))
    (cond ((null? pairs) #f)
          ((ellipsis-pairs? (car pairs))
           (get-ellipsis-first-pair (car pairs)))
          (else (car pairs)))))

(define (assoc-matches patval matches)
  (if (null? matches) #f
      (if (eq? patval (get-matched-pair-patval (car matches)))
          (car matches)
          (assoc-matches patval (cdr matches)))))

(define (remove-matches patval matches)
  (if (null? matches) '()
      (if (eq? patval (get-matched-pair-patval (car matches)))
          (cdr matches)
          (cons (car matches) (remove-matches patval (cdr matches))))))
;; }}}
;; interpreter-syntax-rules-expand-macro {{{
(define (interpreter-syntax-rules-expand-macro expr env literal templates macroenv)
  (let loop ((templates templates))
    (if (null? templates)
        (error "Macro usage is not matched to any pattern")
        (let ((pattern (car (first templates)))
              (template (cadr (first templates)))
              (templates (cdr templates)))
          (let ((matches (try-match literal pattern expr macroenv env)))
            (if matches
                (interpreter-syntax-rules-expand-macro-with-matches matches template env macroenv)
                (loop templates)))))))
;; }}}
;; try-match {{{
(define (try-match literal-identifiers pattern expr macroenv env)
  (define (constant? pattern) ; {{{
    (or (and (atom? pattern) (not (symbol? pattern))) (null? pattern))) ; }}}
  (define (literal-identifier? pattern) ; {{{
    (and (symbol? pattern) (member pattern literal-identifiers))) ; }}}
  (define (ellipsis-pattern? pattern) ; {{{
    (and (pair? pattern)
         (pair? (cdr pattern))
         (eq? (cadr pattern) '...)
         (null? (cddr pattern)))) ; }}}
  (define (merge-matches matches newmatches) ; {{{
    ;; Note that matches are ellipsis pairs.
    (if (null? matches)
        ;; Convert all matched-pair to ellipsis-pairs
        (map (lambda (match)
               (construct-ellipsis-pairs
                 (get-matched-pair-patval match) (list match)))
             newmatches)
        ;; If there are same matches that binds same pattern variable, merge it
        ;; as ellipsis matched pair.
        (let* ((match     (car matches))
               (patval    (get-ellipsis-patval match))
               (pairs     (get-ellipsis-pairs  match))
               (newmatch  (assoc-matches patval newmatches))
               (restmatch (remove-matches patval newmatches)))
          (cons
            (if newmatch
                (construct-ellipsis-pairs patval (append pairs (list newmatch)))
                match)
            (merge-matches (cdr matches) restmatch))))) ; }}}
  (define (wrap-null-ellipsis-patval pattern) ; {{{
    (cond ((literal-identifier? pattern) '())
          ((ellipsis-pattern? pattern)
           (wrap-null-ellipsis-patval (car pattern)))
          ((pair? pattern)
           (append (wrap-null-ellipsis-patval (car pattern))
                   (wrap-null-ellipsis-patval (cdr pattern))))
          ((constant? pattern) '())
          ((not (literal-identifier? pattern))
           (list (construct-ellipsis-pairs pattern '())))
          )) ; }}}

  ;; Begin matching. {{{
  (call/cc
    (lambda (cont)
      (let loop ((pattern pattern) (expr expr))
        (cond 
          ;; If pattern is one of literal identifiers, then it is matched if
          ;; both pattern and expr are not bounded or have the same binding.
          ((literal-identifier? pattern) ; {{{
           (let ((pattern-binding (has-identifier? pattern macroenv))
                 (expr-binding (has-identifier? expr env)))
             (cond
               ;; Both pattern and expr are not bounded.
               ((and (not pattern-binding) (not expr-binding)) '())
               ;; Either pattern or expr is bounded, matching is failed. 
               ((or pattern-binding expr-binding) (cont #f))
               (else
                 (let ((pattern-env (get-defined-env pattern macroenv))
                       (expr-env (get-defined-env expr env)))
                   ;; They are the same binding iff their defined environment is
                   ;; same.
                   (if (eq? pattern-env expr-env)
                     (construct-matched-pair pattern expr)
                     (cont #f))))))) ; }}}

          ;; If pattern is a ellipsis pattern, then matching process is recurred
          ;; while expr is not empty list.
          ((ellipsis-pattern? pattern) ; {{{
           (let ((pattern (car pattern)))
             (let ellipsis-loop ((expr expr) (matches '()))
               (cond ((null? expr)
                      ;; To avoid zero length ellipsis-pattern's variables are
                      ;; treated as literal-identifier, this operation is
                      ;; needed.
                      (if (not (null? matches)) matches
                          (wrap-null-ellipsis-patval pattern)))
                     ((pair? expr)
                      (ellipsis-loop
                        (cdr expr)
                        (merge-matches matches (loop pattern (car expr)))))
                     (else (cont #f)))))) ; }}}

          ;; If pattern is a list or an improper list, then it is matched if
          ;; expr is a list and matched all elements respectively.
          ((pair? pattern) ; {{{
           (if (pair? expr)
               (append (loop (car pattern) (car expr))
                       (loop (cdr pattern) (cdr expr)))
               (else (cont #f)))) ; }}}

          ;; If pattern is a constant expression, then it is matched if expr is
          ;; equal? to pattern.
          ((constant? pattern) ; {{{
           (if (equal? pattern expr) '() (cont #f))) ; }}}

          ;; If pattern is an identifier that is not one of literal identifiers,
          ;; then it is matched.
          ((not (literal-identifier? pattern)) ; {{{
           (list (construct-matched-pair pattern expr))) ; }}}

          ;; Any other pattern is not listed in R5RS.
          (else (cont #f)))))) ; }}}
  )
;; }}}
;; interpreter-syntax-rules-expand-macro-with-matches {{{
(define (interpreter-syntax-rules-expand-macro-with-matches matches template env macroenv)
  (define (constant? template) ; {{{
    (or (and (atom? template) (not (symbol? template))) (null? template))) ; }}}
  (define (ellipsis-template? template) ; {{{
    (and (pair? template)
         (pair? (cdr template))
         (eq? (cadr template) '...))) ; }}}
  (define (first-template template) ; {{{
    (first template)) ; }}}
  (define (rest-template template) ; {{{
    (if (ellipsis-template? template)
        (cddr template)
        (cdr template))) ; }}}
  (define (find-reference ident) ; {{{
    (let ((pair (find-reference-binding ident env)))
      (if (and pair
               (eq? (get-reference-env (cdr pair)) macroenv))
          (car pair)
          #f))) ; }}}
  (define (introduce-reference ident) ; {{{
    (let ((generated-name (find-reference ident)))
      (or generated-name
        (let ((name (gensym)))
          (add-identifier! name env)
          (set-identifier! name (construct-reference ident macroenv) env)
          name)))) ; }}}
  (define (decrease matches) ; {{{
    (map (lambda (match)
           (if (ellipsis-pairs? match)
               (if (null? (get-ellipsis-pairs match))
                   match
                   (construct-ellipsis-pairs (get-ellipsis-patval match)
                                             (cdr (get-ellipsis-pairs match))))
               match))
         matches)) ; }}}

  (define (expand-ellipsis-template matches template) ; {{{
    (let ellipsis-loop ((matches  matches))
      ;; When one of ellipsis-pairs are exhausted, ellipsis-template's expansion
      ;; should be terminated.
      (let ((result (expand matches template)))
        (if result
            (append result (ellipsis-loop (decrease matches)))
            '())))) ; }}}

  (define (expand matches template) ; {{{
    (call/cc
      (lambda (cont)
        (let loop ((template template))
          (cond
            ((symbol? template) ; {{{
             (let ((match (assoc-matches template matches)))
               (if match
                   (if (ellipsis-pairs? match)
                       ;; Check whether ellipsis-pair is not exhausted.
                       (let ((pair (get-ellipsis-first-pair match)))
                         (if pair
                             (list (get-matched-pair-expr pair))
                             (cont #f)))
                       (list (get-matched-pair-expr match)))
                   ;; When template is not one of pattern variables, 
                   (list (introduce-reference template))))) ; }}}
            ((ellipsis-template? template) ; {{{
             (list (append (expand-ellipsis-template matches
                                                     (first-template template))
                           (car (loop (rest-template template)))))) ; }}}
            ((pair? template) ; {{{
             (list (append (loop (first-template template))
                           (car (loop (rest-template  template)))))) ; }}}
            ((constant? template) ; {{{
             (list template)) ; }}}
            ))))) ; }}}

  (car (expand matches template)))
;; }}}

