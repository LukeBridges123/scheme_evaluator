#lang racket
(provide enclosing-environment
         first-frame
         the-empty-environment
         make-frame
         add-binding-to-frame!
         extend-environment
         lookup-variable-value
         set-variable-value!
         define-variable-value!)
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (let ((var-val-pairs (foldl (lambda (var val pairs) (cons (cons var val) pairs))
                              '()
                              variables
                              values)))
    (make-hash var-val-pairs)))
(define (add-binding-to-frame! var val frame)
  (hash-set! frame var val))

(define (extend-environment vars vals base-env)
  (cons (make-frame vars vals) base-env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        #|see if the first frame in the environment contains a binding for the relevant
        variable; if not, tail call env-loop on the rest of the environment.
        Note that the weird hack of wrapping the call to env-loop in a lambda of no arguments is needed
        because, while hash-ref does allow tail-calling a procedure instead of returning a value when
        the given key has no associated value in the given hash table, it only allows procedures of no arguments.
        |#
        (hash-ref (first-frame env) var (lambda ()
                                          (env-loop (cdr env))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (cond
      ((eq? env the-empty-environment) (error "Unbound variable--SET" var))
      ((hash-has-key? (first-frame env) var) (hash-set! (first-frame env) var val))
      (else (env-loop (cdr env)))))
  (env-loop env))

(define (define-variable-value! var val env)
  ;holds onto the first frame in the given environment--it will be needed if no binding for val already exists.
  (let ((original-first-frame (car env)))
    (define (env-loop env)
      (cond
        ((eq? env the-empty-environment) (hash-set! original-first-frame var val))
        ((hash-has-key? (first-frame env) var) (hash-set! (first-frame env) var val))
        (else (env-loop (cdr env)))))
    (env-loop env)))
    

(define test-env (list
                  (make-frame '(square) (list (lambda (x) (* x x))))
                  (make-frame '(a b) '(1 2))
                  (make-frame '(a c) '(3 4))))

(lookup-variable-value 'a test-env)
(lookup-variable-value 'b test-env)
(lookup-variable-value 'c test-env)
(set-variable-value! 'a 5 test-env)
(lookup-variable-value 'a test-env)
;(lookup-variable-value 'd test-env)
;((lookup-variable-value 'square test-env) 5)
(define-variable-value! 'a 2 test-env)
(lookup-variable-value 'a test-env)
(define-variable-value! 'd 6 test-env)
(lookup-variable-value 'd test-env)