#lang racket

(require "enviro.rkt")
(provide startEval repl-eval)

;; Some renaming for readability.
(define second cadr)
(define third caddr)


;; EVAL #########################################################

;; Evaluates a lisp program
;; x -> a lisp program
;; return -> the result of the program
(define (startEval x)
  (my-eval x (environment)))

;; Evaluates a lisp expression or program
;; x -> a lisp expression or program
;; env -> the current evaluation environment
;; return -> the result of the evaluation
(define (my-eval x env)
  (if (not (pair? x))
     (eval-atom x env)
     (eval-function x env)))


;; Evaluates an expression that is made up of a single non-list element
;; x -> a lisp atom
;; env -> the current evaluation environment
;; return -> the result of the evaluation
(define (eval-atom x env)
  (if (symbol? x)
    (let ([ __val (env-lookup x env)])
      (if (unbound? __val)
        (ref-error x)
        __val))
  x))


;; Evaluates a lisp expression that is a function expression
;; x -> a lisp function expression
;; env -> the current evaluation environment
;; return -> the result of the evaluation
(define (eval-function x env)
  (let ([__proc (car x)]
        [__args (cdr x)])
    (cond
     [(pair? __proc)
       ((my-eval __proc env) __args env)]
     [(procedure? __proc)
       (__proc __args env)]
     [(symbol? __proc)
       (let ([__val (env-lookup __proc env)])
         (if (unbound? __val)
           (ref-error x)
           (my-eval (cons __val __args) env)))]
     [else
       (raise-argument-error 'my-eval
                             "a procedure***"
                             (list __proc __args))])))

;; A special function used to start the REPL
(define (repl-eval x env)
  (my-eval x (env-add-bindings env (environment))))


;; BUILTIN ENV #################################################

;; Creates a new enviromnet with all the builtin bindings
(define (environment)
  (env-add-bindings (builtin) (make-env)))

;; Returns a list with all builtin function names and their
;; associated procedures.
;; Any additional structures can be added to an associated symbol
;; in this list and they will become part of the language
(define (builtin)
  (list
    ;; Arithmetic
    (make-binding '+ (binary-op +))
    (make-binding '- (binary-op -))
    (make-binding '* (binary-op *))
    (make-binding '/ (binary-op /))
    
    ;; Comparisson
    (make-binding '= (binary-op =))
    (make-binding '<= (binary-op <=))
    (make-binding '< (binary-op <))
    (make-binding '>= (binary-op >=))
    (make-binding '> (binary-op >))
    (make-binding 'equal? (binary-op equal?))
    (make-binding 'null? (unary-op null?))
    (make-binding 'not (unary-op not))
    
    ;; List
    (make-binding 'pair? (unary-op pair?))
    (make-binding 'cdr (unary-op cdr))
    (make-binding 'car (unary-op car))
    (make-binding 'cons (binary-op cons))
    (make-binding 'list my-list)
    
    ;; Conditional
    (make-binding 'if my-if)
    (make-binding 'cond my-cond)
    
    ;; Other
    (make-binding 'quote my-quote)
    (make-binding 'lambda my-lambda)
    (make-binding 'let my-let)
    (make-binding 'letrec my-letrec)
    (make-binding 'map my-map)
    ))

;; Redefine a given unary procedure to take a list of arguments
;; and a namespace. New procedure calls proc on the first arg.
;; proc -> a lisp procedure
;; return -> the new procedure.
(define (unary-op proc)
  (lambda (x env)
    (proc (my-eval (car x) env))))

;; Same as unary-op but for binary procedures
(define (binary-op proc)
  (lambda (x env)
    (proc (my-eval (car x) env)
          (my-eval (second x) env))))

;; Same as unary-op but for ternary procedures
(define (ternary-op proc)
  (lambda (x env)
    (proc (my-eval (car x) env)
          (my-eval (second x) env)
          (my-eval (third x) env))))


;; VARIABLE BINDINGS ############################################

;; Returns a new list of bindings with all values evaluated
;; x -> a list of var, val tuples
;; env -> the current evaluation environment
;; return -> a list of bindings
(define (eval-bindings bindings env)
  (map (lambda (b)
          (make-binding (binding-symbol b)
                        (my-eval (binding-value b) env)))
       bindings))

;; Error for referencing unbound variables
;; x -> the variable name
(define (ref-error x)
  (raise-syntax-error x
          (string-append "undefined***;\n cannot reference "
                        "an identifer before its definition")))


;; SIMPLE EXPRESSIONS ##########################################

;; Exaluates the arguments to an if function
;; x -> a list of arguments
;; env -> the current evaluation environment
;; return -> the result of the if expression
(define (my-if x env)
  (let ([__cond (car x)]
        [__then (second x)]
        [__else (third x)])
    (if (my-eval __cond env)
      (my-eval __then env)
      (my-eval __else env))))

;; Evaluates the arguments to a quote function
;; x -> a list of arguments
;; env -> the current evaluation environment
;; return -> the quoted result
(define (my-quote x env)
  (quasiquote (unquote (car x))))

;; Evaluates the arguments to a list function
;; x -> a list of arguments
;; env -> the current evaluation environment
;; return -> the new list
(define (my-list x env)
  (map (lambda (y) (my-eval y env)) x))


;; LAMBDA #######################################################

;; Evaluates the arguments to a lambda function
;; x  -> a list of arguments
;; env -> the current evaluation environment
;; returns -> a procedure that will execute the lambda when called
(define (my-lambda x env)
  ;; Create a procedure for the lambda
  (lambda (args __env)
    (let ([__param (car x)]
          ;; Evaluate the arguments with the current namespace
          [__args (map (lambda (x)
                          (my-eval x __env))
                       args)])
      ;; Evaluate the body with the namespace that was current
      ;; when the lambda was created
      (my-eval (second x)
               (env-add-bindings (binding-zip __param __args) env)))))


;LET/LETREC #####################################################

;; Evaluates the arguments to a let function
;; x -> a list of arguments
;; env -> a namespace
;; return -> the result of the let expression
(define (my-let x env)
  (let ([__defs (car x)])
    ;; Evaluate body after adding local bindings to the namespace
    (my-eval (second x)
             (env-add-bindings (eval-bindings __defs env) env))))

;; Evaluates the arguments to a letrec function.
;; x -> a list of arguments
;; env -> a namespace
;; return -> the result of the letrec expression
(define (my-letrec x env)
  (let* ([__defs (car x)]
         ;; Add unevaluated binding to create a new namespace
         [__env (env-add-bindings __defs env)])
    ;; Evaluate body after adding evaluated local bindings to the
    ;; namespace
    (my-eval (second x)
             (env-add-bindings (eval-bindings __defs __env) env))))

;; Evaluates a conditional expression
;; x -> a list of arguments
;; env -> the current evaluation environment
;; return -> the result of the conditional expression
(define (my-cond x env)
  (cond
  [(not (null? x))
    (let* ([__stmt (car x)]
           [__cond (car __stmt)]
           [__body (second __stmt)])
    (if (my-eval __cond env)
      (my-eval __body env)
      (my-cond (cdr x) env)))]))

;; Evaluates the arguments of a map expression
;; x -> a list of arguments
;; env -> the current evaluation environment
;; return -> the result of the map expression
(define (my-map x env)
  (let ([__proc (car x)]
        [__list (second x)])
    (map-rec (my-eval __proc env) (my-eval __list env) env)))

;; Recursive helper for my-map
(define (map-rec proc x env)
  (cond
   [(null? x)
      '()]
   [else
    (cons (proc (list (car x)) env)
          (map-rec proc (cdr x) env))]))
