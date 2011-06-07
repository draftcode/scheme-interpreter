(add-load-path ".")
(use srfi-1)
(require "interpreter/context")
(require "interpreter/support")

(require "interpreter/apply")
(require "interpreter/body")
(require "interpreter/define")
(require "interpreter/define-syntax")
(require "interpreter/environment")
(require "interpreter/functions")
(require "interpreter/if")
(require "interpreter/expr")
(require "interpreter/macro-transformer")
(require "interpreter/syntax-rules")
(require "interpreter/quote.scm")
(require "interpreter/value")

(define (interpreter-program program)
  (let ((env (default-environment))
        (lastexp (construct-undefined)))
    (let loop ()
      (if (null? program) lastexp
          (let ((command (car program)))
            (set! program (cdr program))
            (if (and (list? command) (symbol? (car command)))
                (cond ((eq? (car command) 'define)
                       (set! lastexp (let/ctx '() ctx (interpreter-define command env ctx))))
                      ((eq? (car command) 'begin)
                       (set! program (append (cdr command) program)))
                      ((eq? (car command) 'define-syntax)
                       (set! lastexp (interpreter-define-syntax (second command) (third command) env)))
                      (else
                        (set! lastexp (let/ctx '() ctx (interpreter-expr command env ctx #f)))))
                (set! lastexp (let/ctx '() ctx (interpreter-expr command env ctx #f))))
            #?=lastexp
            (loop))))))

(call-with-input-file "test.scm"
  (lambda (port)
    (let loop ((program '()))
      (let ((expr (read port)))
        (if (eof-object? expr)
            (interpreter-program program)
            (loop (append program (list expr))))))))
