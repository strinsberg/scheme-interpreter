#lang racket
(provide startEval)
(provide repl-eval)
(provide lookup)
(provide UNBOUND)

;CONSTANTS ######################################################

;; A constant for unbound variables. gensym gives it a unique
;; value so it won't clash with any other variables.
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
   ;; x is a single peice of data
   [(not (pair? x))
     (if (symbol? x)
       (let ([ __val (lookup x ns)])
          (if (equal? __val UNBOUND)  ;; Error if it has no value
            (ref-error x)
            __val))
       x)]
   
   ;; x is a function. Its first arg should eval to a procedure
   [(pair? (car x))  ;; x's first arg is a function
     ((my-eval (car x) ns) (cdr x) ns)]

   [(procedure? (car x))  ;; x' first arg is a procedure
     ((car x) (cdr x) ns)]
   
   [else  ;; x's first arg is a variable
     (let ([__val (lookup (car x) ns)])  ;; get value
       (cond
        [(equal? __val UNBOUND)  ;; variable has no value
          (ref-error (car x))]
          
        [(procedure? __val)  ;; variable represents a procedure
           (__val (cdr x) ns)]
           
        [(pair? __val)  ;; variable represents a function
           ((my-eval __val ns) (cdr x) ns)]
           
        [(symbol? __val)  ;; variable points to another varaible
           ((my-eval (lookup __val ns) ns) (cdr x) ns)]
          
        [else  ;; variable is not a procedure
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

;; Redefine a given unary procedure to take a list of arguments
;; and a namespace. New procedure calls proc on the first arg.
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
;; return -> the bound value or UNBOUND
(define (lookup v ns)
  (cond
   [(null? ns)
      UNBOUND]
   [(equal? (caar ns) v)
      (second (car ns))]
   [else
      (lookup v (cdr ns))]))

;; Bind 2 lists of variables and values into a list of var, val
;; tuples. If list are not the same length extras are discarded.
;; x -> a list of variable names
;; y -> a list of values
;; return -> a list of var, value tuples
(define (bind x y)
  (if (or (null? x) (null? y))
    '()
    (cons (list (car x)
                (car y))
          (bind (cdr x) (cdr y)))))

;; Evaluates and rebinds the values in a list of variable, value
;; tuples.
;; x -> a list of var, val tuples
;; ns -> a namespace
;; return -> a list with variable and evaluated value tuples
(define (eval-values x ns)
  (map (lambda (y)
          (list (car y)
                (my-eval (second y) ns)))
       x))

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
  ;; Create a procedure for the lambda
  (lambda (args __ns)
    (let ([__param (car x)]
          ;; Evaluate the arguments with the current namespace
          [__args (map (lambda (x)
                          (my-eval x __ns))
                       args)])
      ;; Evaluate the body with the namespace that was current
      ;; when the lambda was created
      (my-eval (second x)
               (append (bind __param __args) ns)))))


;LET/LETREC #####################################################
;; See report for additional documentation

;; Evaluates the arguments to a let function
;; x -> a list of arguments
;; ns -> a namespace
;; return -> the result
(define (my-let x ns)
  (let ([__defs (car x)])
    ;; Evaluate body after adding local bindings to the namespace
    (my-eval (second x)
             (append (eval-values __defs ns) ns))))

;; Evaluates the arguments to a letrec function.
;; x -> a list of arguments
;; ns -> a namespace
;; return -> the result
(define (my-letrec x ns)
  (let* ([__defs (car x)]
         ;; Add unevaluated binding to create a new namespace
         [__ns (append __defs ns)])
    ;; Evaluate body after adding evaluated local bindings to the
    ;; namespace
    (my-eval (second x)
             (append (eval-values __defs __ns) ns))))