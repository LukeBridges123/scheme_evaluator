#lang racket


;syntax

;only numbers and strings are considered self-evaluating. Will use the underlying Racket version of both types.
(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)))
(define (variable? exp)
  (symbol? exp))
;tagged-list?: one of the key functions for identifying expressions as being of some type. Answers whether exp
;is a list whose first item is equal to the argument "tag".
(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
#|(define... could be followed by either a symbol (in which case we're defining an ordinary variable e.g.
(define x 2) or a list (in which case we're defining a procedure, e.g. (define (square x)... and the identifier
we should extract is the caadr (car of the cadr, as in this case the cadr will be (square x) or whatever) of exp.
Similarly, the value will be just the value of the caddr, in the first case, or else a lambda needs to be made
from the parameters (the cdr of the cadr, i.e. cdadr) and the procedure body (everything minus "define" and the cadr,
so the cdr of the cdr, i.e. cddr)
|#
(define (definition-variable exp)
  (if (symbol? (cadr exp)) (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp)) (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
#|
lambda expression has the form (lambda (arg1 arg2...) (body-expr-1) (body-expr-2) ...)
So it's a tagged list with lambda as the tag, the parameters are the cadr of the list, and
the body is the cddr.
|#
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) (list 'lambda parameters body))

#|
if expressions are of the form (if predicate consequent alternative), naturally suggesting cadr, caddr, and cadddr as selectors
for the predicate, consequent, and alternative, respectively. We allow for the possibility that there is no alternative in which case,
when the predicate is false, the if expression as a whole should evaluate to false.
|#
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (null? (cdddr exp)) '#f
      (cadddr exp)))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
#|
begin expressions are of the form (begin exp1 exp2 ... ) where the value of the expression as a whole is the value of the last expression.
The functions below give some syntactic sugar for the basic list operations, to be used in various places where begin expressions are used,
as well as a function last-exp? which determines whether a sequence of expressions contains only one expression (to be used, as described
above, to figure out when an expression whose value must be returned has been reached when evaluating a begin expression).
|#
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr exp)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
#|
In some parts of the evaluator, especially when converting cond expressions (where the consequent part of each (predicate consequence) clause
can be a sequence of expressions) to if expressions (where the consequent of each (if predicate consequent alternative) is a single expression),
we want to be able to "package" a sequence of expressions into a single expression. The following functions do this: an empty sequence of expressions
is not transformed at all, a sequence of a single expression has that expression extracted, while sequences of multiple expressions are turned into
a single begin expression.
|#
(define (make-begin seq) (cons 'begin seq))
(define (sequence->exp seq)
  (cond
    ((null? seq) seq)
    ((last-exp? seq) (first-exp seq))
    (else (make-begin seq))))

#|
Misc. functions for dealing with expressions consisting of a function applied to some arguments. Note that the application? function would in principle
consider things like a quoted expression e.g. (define x 2) to be an application; as such, it's important that eval only checks whether something is an
application after checking whether it's one of those special forms.
|#
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
;eval and apply
(define (eval exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))
    ((if? exp) (eval-if exp env))
    ((lambda? exp) (make-procedure (lambda-parameters exp)
                                   (lambda-body exp)
                                   env))
    ((begin? exp) (eval-sequence (begin-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))
    ((application? exp) (apply (eval (operator exp) env)
                               (list-of-values (operands exp)
                                               env)))
    (else (error "Unknown expression type: EVAL " exp))))

(define (apply proc args)
  (cond
    ((primitive-procedure? proc) (apply-primitive proc args))
    ((compound-procedure? proc) (eval-sequence
                                 (procedure-body proc)
                                 (extend-environment
                                  (procedure-parameters proc)
                                  args
                                  (procedure-environment proc))))
    (else (error "Unknown procedure type: APPLY " proc))))
;auxilliary procedures for eval and apply
(define (list-of-values exps env)
  (map (lambda (exp) (eval exp env)) exps))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond
    ((last-exp? exps) (eval (first-exp exps) env))
    (else (eval (first-exp exps)
                (eval-sequence (rest-exps exps) env)))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assigment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

    
    