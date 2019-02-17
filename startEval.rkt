#lang racket
(provide startEval)
(provide repl-eval)
(provide lookup)
(provide UNBOUND)

;CONSTANTS ######################################################

;; A value for declared but unitialized variables in letrec
;; gensym makes sure that it is a unique value so it won't clash
;; with anything in the program or evaluated program.
(define UNBOUND (gensym))

;; Some renaming for readability.
(define second cadr)
(define third caddr)


;; EVAL #########################################################

;; Evaluates a racket program
;; x -> a racket program
;; return -> the result of the program
(define (startEval x)
  (my-eval x (builtin)))

;; Evaluates a racket program with a given namepsace added to
;; the builtin namespace. The new namespace is added so that
;; its binding come before any built-in bindings in lookups.
;; x -> a racket program
;; ns -> a namespace
;; return -> the result of the program
(define (repl-eval x ns)
  (my-eval x (append ns (builtin))))

;; Evaluates a racket expression or program
;; x -> a racket expression or program
;; return -> the result of the evaluation
(define (my-eval x ns)
  (cond
   ;; x is a single data value
   [(not (pair? x))
     (if (symbol? x)
       (let ([ __val (lookup x ns)])
          (if (equal? __val UNBOUND)
            (ref-error x)
            __val))
       x)]
   
   ;; x is a function
   ;; the procedure for x is a function
   [(pair? (car x))
     ((my-eval (car x) ns) (cdr x) ns)]
     
   ;; the procedure for x is a procedure
   [(procedure? (car x))
     ((car x) (cdr x) ns)]
   
   ;; the procedure for x is a variable
   [else
     ;; get the value for the variable
     (let ([__val (lookup (car x) ns)])
       (cond
        [(equal? __val UNBOUND)
          (ref-error (car x))]
        ;; value is a procedure
        [(procedure? __val)
           (__val (cdr x) ns)]
           
        ;; value is a function
        [(pair? __val)
           ((my-eval __val ns) (cdr x) ns)]
           
        ;; value is another variable
        [(symbol? __val)
           ((my-eval (lookup __val ns) ns) (cdr x) ns)]
           
        ;; value is not a procedure
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
;; a list of arguments and a namespace. When called the new
;; procedure calls proc on the evaluated value of the first
;; argument. Aditional arguments are discarded.
;; proc -> a racket procedure
;; return -> the new procedure.
(define (unary-op proc)
  (lambda (x ns)
    (proc (my-eval (car x) ns))))

;; Same as unary-op but for binary procedures
(define (binary-op proc)
  (lambda (x ns)
    (proc (my-eval (car x) ns)
          (my-eval (second x) ns))))

;; Same as unary-op but for ternary procedures
(define (ternary-op proc)
  (lambda (x ns)
    (proc (my-eval (car x) ns)
          (my-eval (second x) ns)
          (my-eval (third x) ns))))


;; VARIABLE BINDINGS ############################################

;; Looks up a given variables in a given namespace
;; v -> a variable name
;; ns -> a namespace
;; returns -> the bound value or UNBOUND
(define (lookup v ns)
  (cond
   [(null? ns)
      UNBOUND]
   [(equal? (caar ns) v)
      (second (car ns))]
   [else
      (lookup v (cdr ns))]))

;; Error for referencing unbound variables
;; x -> the variable name
(define (ref-error x)
  (raise-syntax-error x
          (string-append "undefined***;\n cannot reference "
                        "an identifer before its definition")))


;SIMPLE EXPRESSIONS #############################################

;; Exaluates the arguments to an if function
;; x -> a list of arguments
;; ns -> a namespace
;; return -> the result
(define (my-if x ns)
  (let ([__cond (car x)]
        [__then (second x)]
        [__else (third x)])
    (if (my-eval __cond ns)
      (my-eval __then ns)
      (my-eval __else ns))))

;; Evaluates the arguments to a list function
;; x -> a list of arguments
;; ns -> a namespace
;; return -> the result
(define (my-list x ns)
  (map (lambda (x)
          (my-eval x ns))
        x))

;; Evaluates the arguments to a quote function
;; x -> a list of arguments
;; ns -> a namespace
;; return -> the result
(define (my-quote x ns)
  (quasiquote (unquote (car x))))


;; LAMBDA #######################################################
;; See report for additional documentation

;; Evaluates the arguments to a lambda function
;; x  -> a list of arguments
;; ns -> a namespace
(define (my-lambda x ns)
  ;; Create a procedure to be called when the lambda procedure
  ;; would be evaluated.
  (lambda (args __ns)
    (let ([__param (car x)]
          ;; Evaluate the arguments to the lambda call with
          ;; the current namespace.
          [__args (eval-args args __ns)])
      ;; Evaluate the body with a namespace that has the original
      ;; lambdas parameters assigned to the arguments passed to
      ;; the call of this procedure. Use the namespace that was
      ;; current when this procedure was created. Do not evaluate
      ;; the arguments as they were already evaluated with their
      ;; proper namespace.
      (my-eval (second x)
               (assign __param __args ns #f)))))

;; Evaluate a list of arguments.
;; args -> a list of arguments to a lambda
;; ns -> the current namespace
;; returns -> a list of the results
(define (eval-args args ns)
  (map (lambda (x)
          (my-eval x ns))
       args))

;; Binds vals to vars and construct a new namespace with the
;; var val pairs and the given namespace.
;; vars -> a list of variable names
;; vals -> a list of values
;; ns -> the current namespace
;; eval? -> wether or not to evaluate the values before
;; binding them to the variables
;; returns -> the new namespace
(define (assign vars vals ns eval?)
  (letrec ([rec (lambda (x y new-ns)
                  ;; If either is null return the new namespace
                  (if (or (null? x) (null? y))
                    new-ns
                    ;; Otherwise make a 2 element list to bind the
                    ;; first variable in x to the first value in y
                    ;; and cons then onto the new namespace to
                    ;; use in the recursive call on teh rest of
                    ;; x and y
                    (rec (cdr x)
                         (cdr y)
                         (cons (list (car x)
                                     ;; If desired evaluate the
                                     ;; value before binding it
                                     (if eval?
                                        (my-eval (car y) ns)
                                        (car y)))
                               new-ns))))])
    (rec vars vals ns)))
  

;LET/LETREC #####################################################
;; See report for additional documentation

;; Evaluates the arguments to a let function
;; x -> a list of arguments
;; ns -> a namespace
;; return -> the result
(define (my-let x ns)
  (let ([__defs (car x)]
        [__body (cdr x)])
    ;; Evaluate the body and pass it a namespace that has the
    ;; local variables and their evaluated values.
    (my-eval (second x)
             (assign (map car __defs)
                     (map second __defs)
                     ns
                     #t))))

;; Evaluates the arguments to a letrec function.
;; x -> a list of arguments
;; ns -> a namespace
;; return -> the result
(define (my-letrec x ns)
  (let* ([__defs (car x)]
         ;; Create a new namespace with the definition variables
         ;; and their unevaluated values.
         [__ns (assign (map car __defs)
                       (map second __defs)
                       ns
                       #f)])
    ;; Return the result of evaluating the lambda body, but this
    ;; time use the namespace with unevaluated values and
    ;; re-assign the values but this time evaluate them.
    (my-eval (second x)
             (assign (map car __defs)
                     (map second __defs)
                     __ns
                     #t))))