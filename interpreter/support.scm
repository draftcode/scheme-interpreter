(define (atom? v)
  (and (not (pair? v)) (not (null? v))))

(define (get-referenced-value val)
  (if (not (is-reference? val)) val
      (find-in-environment (get-reference-ident val)
                           (get-reference-env val))))
;; find-in-environment ... リファレンスを外しながら真の値を探す. {{{
(define (find-in-environment ident env)
  (let ((val (get-identifier ident env)))
    (if (and val (is-reference? val))
        (find-in-environment (get-reference-ident val)
                             (get-reference-env val))
        val)))
;; }}}
;; find-original-name {{{
(define (find-original-name ident env)
  (let ((val (get-identifier ident env)))
    (cond ((not val) ident)
          ((is-reference? val)
           (find-original-name (get-reference-ident val)
                               (get-reference-env val)))
          (else 'nowhere))))
;; }}}

