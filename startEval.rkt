#lang racket
(provide startEval)  ;; Make startEval available when required
;(provide repl-eval)  ;; Make this available for REPL use

;; NOTE all non-trivial functions and algorithms are
;; more fully documented in the report. This is done to keep
;; the code more consise and not to seperate parts of an algorithm
;; with large blocks of comments.

;; TODO list does not properly return a 'c as a symbol it returns
;; it as the variable c. Racket somehow does something under
;; the hood that I don't understand because the way it treats an
;; evaluated 'c and a given c are different even though they
;; look the same when printed.
;; > (list 3 4 5 'c)
;; '(3 4 5 c)
;; > (map symbol? (list 3 4 5 c))
    ; c: undefined;
    ;  cannot reference an identifier before its definition
    ;   in module: top-level
    ; [,bt for context]
;; > (map symbol? (list 3 4 5 'c))
;; '(#f #f #f #t)  ;; my-eval will throw the above error for this

;; TODO lambda let and letrec all have very similar code, if it
;; is possible you could try to extract it to a function or two
;; TODO think about how you would go about passing the namespace
;; around rather than using a mutable hash. Pass the list to
;; all calls to my-eval. When putting things onto it,
;; (cons vars onto it). no need to pop anything because every
;; scope has it's own copy of the variables it need.

;CONSTANTS ######################################################

;; A value for declared but unitialized variables in letrec
(define UNBOUND (gensym))

;; Some renaming for readability
(define second cadr)
(define third caddr)


;; EVAL #########################################################

;; Evaluates a racket program
;; x -> a racket program
;; Returns the result of the program
(define (startEval x)
  (my-eval x (builtin)))

;; Evaluates a racket expression or program
;; x -> a racket expression or program
;; Returns the result of the evaluation
(define (my-eval x stack)
  (cond
  ;If x is data return it or its bound value if it has one
  [(not (pair? x))
    (if (symbol? x)  ;; If it isn't a symbol skip lookup
      (let ([__val (lookup x stack)])
        (cond
        [(equal? __val UNBOUND)
          (ref-error 'my-eval-data x)]
        [#t
          __val]))
      x)]
  ;; x is a function and its procedure is also a function
  ;; (an anonymus lambda, etc)
  [(pair? (car x))
    ((my-eval (car x) stack) (cdr x) stack)]
  ;; First element of x is an actual procedure then just call
  ;; it on a list of its arguments. This really only applies to
  ;; things that are saved in function calls for later use.
  [(procedure? (car x))
    ((car x) (cdr x) stack)]
  ;; Else x is a function and (car x) is a variable.
  [else
    (let ([__val (lookup (car x) stack)])  ;; get variables value
      (cond
       [(equal? __val UNBOUND)  ;; variable is unbound
          (ref-error 'my-eval-procedure (car x))]
       [(procedure? __val)  ;; variable is a proceduer
          (__val (cdr x) stack)]
       [(pair? __val)
          ((my-eval __val stack) (cdr x) stack)]
       [(symbol? __val)
          ((my-eval (lookup __val stack) stack) (cdr x) stack)]
       [else  ;; first element of function is not a procedure
          (raise-argument-error 'my-eval
                                "a procedure***"
                                 __val)]))]))


;; BUILTINS #####################################################

;; Returns a hash table with all builtin function names and their
;; associated procedures.
(define (builtin)
  (list
    ;; Arithmetic
    (list '+ (binary-op +))
    (list '- (binary-op -))
    (list '* (binary-op *))
    (list '/ (binary-op /))
    
    ;; Comparisson
    (list '= (binary-op =))
    (list '<= (binary-op <=))
    (list '< (binary-op <))
    (list '>= (binary-op >=))
    (list '> (binary-op >))
    (list 'equal? (binary-op equal?))
    (list 'null? (unary-op null?))
    
    ;; List
    (list 'pair? (unary-op pair?))
    (list 'cdr (unary-op cdr))
    (list 'car (unary-op car))
    (list 'cons (binary-op cons))
    (list 'list (lambda (x stack) (map (lambda (x) (my-eval x stack)) x)))
    
    ;; Conditional
    (list 'if my-if)
    
    ;; Other
    (list 'quote (lambda (x stack)
                    (quasiquote (unquote (car x)))))
    (list 'lambda my-lambda)
    (list 'let my-let)
    (list 'letrec my-letrec)
    ))

;; Redefine a given unary procedure to be a procedure that takes
;; a list of arguments and uses the first one. The new procedure
;; discards any additional arguments.
;; proc -> a racket procedure
;; Returns the new procedure.
(define (unary-op proc)
  (lambda (x stack)
    (proc (my-eval (car x) stack))))

;; Same as unary-op but the resulting procedure uses the
;; first 2 arguments.
(define (binary-op proc)
  (lambda (x stack)
    (proc (my-eval (car x) stack) (my-eval (second x) stack))))

;; Same as unary-op but for procedures that take 3 arguments
(define (ternary-op proc)
  (lambda (x stack)
    (proc (my-eval (car x) stack)
          (my-eval (second x) stack)
          (my-eval (third x) stack))))


;; VARIABLE BINDINGS #############################################

;; Looks for variable in the stack
(define (lookup v stack)
  ;(println v)
  ;(println stack)
  (letrec ([f (lambda (x)
                (cond
                [(null? x)
                  UNBOUND]
                [(equal? (car (car x)) v)
                  ;(println (car x))
                  (cadr (car x))]
                [else
                  (f (cdr x))]))])
      (f stack)))

;; Raises an error for variables that are referenced before they
;; they have been declared
;; x -> the variable that caused the problem
(define (ref-error loc x)
  (raise-syntax-error x
          (string-append "undefined***;\n cannot reference "
                        "an identifer before its definition")))


;SIMPLE EXPRESSIONS #############################################

;; Evaluates an if expresion
;; x -> a list of arguments to an if expression
;; Returns the result of applying if to the first 3 arguments
(define (my-if x stack)
  (let ([__cond (car x)]
        [__then (second x)]
        [__else (third x)])
    (if (my-eval __cond stack)
      (my-eval __then stack)
      (my-eval __else stack))))


;; LAMBDA ########################################################
;; See report for additional documentation

;; Evaluates a lambda expression
;; x -> a list of arguments to a lambda expression
;; Returns a proceudeure that takes a list of arguments
(define (my-lambda x stack)
  ;; Create and return the new procedure
  (lambda (args _s)
    (let* ([__param (car x)]
           [__body (cdr x)]
           [__args (eval-args args _s)])
        ;(println args)
        ;(println __args)
        ;(println (assign __param __args stack #f))
        (my-eval (car __body) (assign __param __args stack #f)))))

;; Evaluate all the args with a given stack
(define (eval-args args stack)
  (map (lambda (x)
          (my-eval x stack))
       args))

;; assign vals to vars and put them onto the stack
;; returns the new stack. All evaluating done during the assign
;; uses the stack before things are added to it.
(define (assign vars vals stack ev)
  (letrec ([rec (lambda (g y st)
                  (if (or (null? g) (null? y))
                    st
                    (rec (cdr g)
                         (cdr y)
                         (cons (list (car g)
                                     (if ev
                                        (my-eval (car y) stack)
                                        (car y)))
                               st))))])
    (rec vars vals stack)))
  

;LET/LETREC #####################################################
;; See report for additional documentation

;; Evaluates a let expression
;; x -> a list of the arguments to a let expression
;; Returns the result of the last expression in the let body
(define (my-let x stack)
  (let ([__defs (car x)]
        [__body (cdr x)])
    (my-eval (car __body)
             (assign (map car __defs)
                     (map second __defs)
                     stack
                     #t))))

;; Evaluates letrec expressions
;; x -> a list of the arguments to a letrec expression
;; Returns the result of the last expression in the letrec body
(define (my-letrec x stack)
  (let* ([__defs (car x)]
        [__body (cdr x)]
        [__stack (assign (map car __defs)
                              (map second __defs)
                              stack
                              #f)])
    (my-eval (car __body)
             (assign (map car __defs)
                     (map second __defs)
                     __stack
                     #t))))