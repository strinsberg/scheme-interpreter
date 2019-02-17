#lang racket
(provide startEval)  ;; Make startEval available when required

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
;; gensym makes sure that it is a unique value so it won't clash
;; with anything in the program or evaluated program.
(define UNBOUND (gensym))

;; Some renaming for readability.
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
   ;; x is a single data value
   [(not (pair? x))
     (if (symbol? x)
       (lookup x ns)
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
;; Returns the new procedure.
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
;; v  -> a variable name
;; ns -> a namespace
;; Returns the associated value or UNBOUND
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

;; Error for referencing unbound variables
;; x -> the variable name
(define (ref-error x)
  (raise-syntax-error x
          (string-append "undefined***;\n cannot reference "
                        "an identifer before its definition")))


;SIMPLE EXPRESSIONS #############################################

;; if
;; x  ->
;; ns -> a namespace
(define (my-if x ns)
  (let ([__cond (car x)]
        [__then (second x)]
        [__else (third x)])
    (if (my-eval __cond ns)
      (my-eval __then ns)
      (my-eval __else ns))))

;; list
;; x  ->
;; ns -> a namespace
(define (my-list x ns)
  (map (lambda (x)
          (my-eval x ns))
        x))

;; quote
;; x  ->
;; ns -> a namespace
(define (my-quote x ns)
  (quasiquote (unquote (car x))))


;; LAMBDA #######################################################
;; See report for additional documentation

;; lambda
;; x  ->
;; ns -> a namespace
(define (my-lambda x ns)
  (lambda (args _s)
    (let ([__param (car x)]
          [__args (eval-args args _s)])
      (my-eval (second x)
               (assign __param __args ns #f)))))

;; Evaluate a list of arguments and return a list of the evaluated
;; results
;; args -> a list of arguments to a lambda
;; ns   -> the current namespace
(define (eval-args args ns)
  (map (lambda (x)
          (my-eval x ns))
       args))

;; assign vals to vars and put them onto the ns
;; returns the new ns.
;; vars  -> a list of variable names
;; vals  -> a list of values
;; ns    -> the current namespace
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

;; let
;; x  ->
;; ns -> a namespace
(define (my-let x ns)
  (let ([__defs (car x)]
        [__body (cdr x)])
    (my-eval (second x)
             (assign (map car __defs)
                     (map second __defs)
                     ns
                     #t))))

;; letrec
;; x  ->
;; ns -> a namespace
(define (my-letrec x ns)
  (let* ([__defs (car x)]
         [__ns (assign (map car __defs)
                       (map second __defs)
                       ns
                       #f)])
    (my-eval (second x)
             (assign (map car __defs)
                     (map second __defs)
                     __ns
                     #t))))