#lang racket
(provide startEval)  ;; Make startEval available when required
;(provide repl-eval)  ;; Make this available for REPL use

;; NOTE all non-trivial functions and algorithms are
;; more fully documented in the report. This is done to keep
;; the code more consise and not to seperate parts of an algorithm
;; with large blocks of comments.

;; TODO clean up the my-eval function now that it is pretty messy
;; from adding some more cases and such to it.

;; TODO see if you can clean up all the ns passing and my-eval
;; calls. Maybe you can evaluate arguments in my-eval somewhere
;; and only pass them to funcitons once they are evaluated fully

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
(define (my-eval x ns)
  (cond
   [(not (pair? x))
     (if (symbol? x)
       (lookup x ns)
       x)]
   [(pair? (car x))
     ((my-eval (car x) ns) (cdr x) ns)]
   [(procedure? (car x))
     ((car x) (cdr x) ns)]
   [else
     (let ([__val (lookup (car x) ns)])
       (cond
        [(procedure? __val)
           (__val (cdr x) ns)]
        [(pair? __val)
           ((my-eval __val ns) (cdr x) ns)]
        [(symbol? __val)
           ((my-eval (lookup __val ns) ns) (cdr x) ns)]
        [else
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
    (list 'list my-list)
    
    ;; Conditional
    (list 'if my-if)
    
    ;; Other
    (list 'quote my-quote)
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
  (lambda (x ns)
    (proc (my-eval (car x) ns))))

;; Same as unary-op but the resulting procedure uses the
;; first 2 arguments.
(define (binary-op proc)
  (lambda (x ns)
    (proc (my-eval (car x) ns)
          (my-eval (second x) ns))))

;; Same as unary-op but for procedures that take 3 arguments
(define (ternary-op proc)
  (lambda (x ns)
    (proc (my-eval (car x) ns)
          (my-eval (second x) ns)
          (my-eval (third x) ns))))


;; VARIABLE BINDINGS #############################################

;; Looks up variables in a list of variable value paris
;; v     -> the variable name
;; ns -> the list of var val pairs
;; Returns the value if the variable is in the list, or UNBOUND
(define (lookup v ns)
  (letrec ([f (lambda (x)
                (cond
                 [(null? x)
                   UNBOUND]
                 [(equal? (caar x) v)
                   (second (car x))]
                 [else
                   (f (cdr x))]))])
      (f ns)))

;; Raises an error for variables that are referenced before they
;; they have been declared
;; x -> the variable that caused the problem
(define (ref-error x)
  (raise-syntax-error x
          (string-append "undefined***;\n cannot reference "
                        "an identifer before its definition")))


;SIMPLE EXPRESSIONS #############################################

;; Evaluates an if expresion
;; x -> a list of arguments to an if expression
;; Returns the result of applying if to the first 3 arguments
(define (my-if x ns)
  (let ([__cond (car x)]
        [__then (second x)]
        [__else (third x)])
    (if (my-eval __cond ns)
      (my-eval __then ns)
      (my-eval __else ns))))

(define (my-list x ns)
  (map (lambda (x)
          (my-eval x ns))
        x))

(define (my-quote x ns)
  (quasiquote (unquote (car x))))

;; LAMBDA ########################################################
;; See report for additional documentation

;; Evaluates a lambda expression
;; x -> a list of arguments to a lambda expression
;; Returns a proceudeure that takes a list of arguments
(define (my-lambda x ns)
  (lambda (args _s)
    (let ([__param (car x)]
          [__args (eval-args args _s)])
      (my-eval (second x)
               (assign __param __args ns #f)))))

;; Evaluate a list of arguments and return a list of the evaluated
;; results
;; args -> a list of arguments to a lambda
;; ns -> the current namespace
(define (eval-args args ns)
  (map (lambda (x)
          (my-eval x ns))
       args))

;; assign vals to vars and put them onto the ns
;; returns the new ns.
;; vars  -> a list of variable names
;; vals  -> a list of values
;; ns -> the current namespace
;; eval? -> wether or not to evaluate the values before
;; binding them to the variables
(define (assign vars vals ns eval?)
  (letrec ([rec (lambda (g y new-ns)
                  (if (or (null? g) (null? y))
                    new-ns
                    (rec (cdr g)
                         (cdr y)
                         (cons (list (car g)
                                     (if eval?
                                        (my-eval (car y) ns)
                                        (car y)))
                               new-ns))))])
    (rec vars vals ns)))
  

;LET/LETREC #####################################################
;; See report for additional documentation

;; Evaluates a let expression
;; x -> a list of the arguments to a let expression
;; Returns the result of the last expression in the let body
(define (my-let x ns)
  (let ([__defs (car x)]
        [__body (cdr x)])
    (my-eval (car __body)
             (assign (map car __defs)
                     (map second __defs)
                     ns
                     #t))))

;; Evaluates letrec expressions
;; x -> a list of the arguments to a letrec expression
;; Returns the result of the last expression in the letrec body
(define (my-letrec x ns)
  (let* ([__defs (car x)]
        [__body (cdr x)]
        [__ns (assign (map car __defs)
                      (map second __defs)
                      ns
                      #f)])
    (my-eval (car __body)
             (assign (map car __defs)
                     (map second __defs)
                     __ns
                     #t))))