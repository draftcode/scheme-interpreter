
(define (interpreter-quote data) ; {{{
  (cond
    ((boolean? data) (construct-boolean data))
    ((symbol?  data) (construct-symbol  data))
    ((char?    data) (construct-char    data))
    ((vector?  data) (construct-vector  data))
    ((null?    data) (construct-null))
    ((number?  data) (construct-number  data))
    ((string?  data) (construct-string  data))
    ((pair?    data) (construct-pair    (interpreter-quote (car data))
                                        (interpreter-quote (cdr data)))))) ; }}}
(define (interpreter-unquote data) ; {{{
  (cond
    ((is-boolean? data) (get-boolean-value data))
    ((is-symbol?  data) (get-symbol-value  data))
    ((is-char?    data) (get-char-value    data))
    ((is-vector?  data) (get-vector-value  data))
    ((is-number?  data) (get-number-value  data))
    ((is-string?  data) (get-string-value  data))
    ((is-null?    data) '())
    ((is-pair?    data)
     (cons (interpreter-unquote (get-pair-car data))
           (interpreter-unquote (get-pair-cdr data)))))) ; }}}

