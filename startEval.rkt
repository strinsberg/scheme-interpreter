#lang racket
(provide startEval)

;NOTE all arithmetic and relational operations are binary.
;if fewer than 2 arguments are given it will be an error, if more
;arguments are given they will be discarded.

;NOTE I have implemented checks to make sure that variables are
;not referenced before they are assigned to. This is not
;strictly necessary since we expect a valid racket program,
;but it felt like a good thing for an interpreter to do. I also
;replace any variables that have already got values. These things
;prevent dynamic scopeing and allow for some situations where
;lambdas return lambdas that use the outer lambdas parameters.

;TODO clean up all comments and make sure function documentation
;is concise and proper.

;CONSTANTS #######################################################

;A value for declared but unitialized variables in letrec
(define UN_INIT 'uninit)


;EVAL ############################################################

;Evaluates a racket program
;x -> a racket program
;Returns the result of the program
(define (startEval x)
  (push (builtin))
  (my-eval x))

;Evaluates a racket expression/program
;x -> a racket expression
;Returns the result of the expression
(define (my-eval x)
  (cond
  ;If x is a symbol look its value up on the stack. If it exists
  ;return it. Otherwise lookup will raise an error.
  [(symbol? x)
    (lookup x)]
  ;If x is not a pair then it should be a single data type. So
  ;we just return this value.
  [(not (pair? x))
    x]
  ;If x is a pair then it's first element should be a procedure.
  ;If the first element is a pair, then that element is a function
  ;that returns a procedure. So evaluate it as such.
  [(pair? (car x))
    (func-expr x)]
  ;Otherwise if first element is not a pair then it is a single
  ;data type so look it up in the symbol table. If its value
  ;is a procedure run it on the list of arguments. Otherwise
  ;raise an error, because the first element of a function should
  ;always be a procedure
  [else
    (let ([v (lookup (car x))])
      (if (procedure? v)
        (v (cdr x))
        (raise
          (format
            "Error: expected a procedure\n  given: ~a"
            (car x)))))]))


;BUILTINS ########################################################

;; Returns a hash table with all builtin function names and their
;; procedures.
(define (builtin)
  (hash
    'cdr (unary-op cdr)
    'car (unary-op car)
    'null? (unary-op null?)
    'pair? (unary-op pair?)
    '+ (binary-op +)
    '- (binary-op -)
    '* (binary-op *)
    '/ (binary-op /)
    '= (binary-op =)
    '<= (binary-op <=)
    '< (binary-op <)
    '>= (binary-op >=)
    '> (binary-op >)
    'cons (binary-op cons)
    'equal? (binary-op equal?)
    'quote (lambda (x) (quasiquote (unquote (car x))))
    'list (lambda (x) (map my-eval x))
    'if my-if
    'lambda my-lambda
    'let my-let
    'letrec my-letrec))

;Redefine a given unary procedure to be a procedure that takes
;a list of arguments and uses the first one. The new procedure
;discards any additional arguments.
;proc -> a racket procedure
(define (unary-op proc)
  (lambda (x)
    (proc (my-eval (car x)))))

;Same as above but the resulting procedure uses the
;first 2 arguments.
;proc -> a racket procedure
(define (binary-op proc)
  (lambda (x)
    (proc (my-eval (car x)) (my-eval (cadr x)))))


;VARIABLE BINDINGS ##############################################

;Stack for local variable hash tables
(define stack '())

;Push a hash-table of variable value pairs onto the stack
;x -> a hash table of variable names and their values
(define (push x)
  (set! stack (cons x stack)))

;Pops the top off the stack
(define (pop)
  (set! stack (cdr stack)))

;Looks for variable in the local variable list and returns the
;value of that variable if it is found
;Otherwise raises a error for an unbound identifier
;v -> a variable name
(define (lookup v)
  (letrec ([f (lambda (x)
                (cond
                [(null? x)
                  (ref-error v)]
                [(hash-has-key? (car x) v)
                  (hash-ref (car x) v)]
                [else
                  (f (cdr x))]))])
      (f stack)))

;Raises an error for variables that are referenced before they
;they have been declared
;x -> the name of the variable that caused the problem
(define (ref-error x)
  (raise (format "Error: ~a: unbound identifier" x)))


;SIMPLE EXPRESSIONS #############################################

;Rules for evaluating if expresion
;x -> a list of arguments
;ie) (if (x) #t #f)
;Returns the result of applying if to the first 3 arguments
(define (my-if x)
  (let ([__cond (car x)]
        [__then (cadr x)]
        [__else (caddr x)])
    ;Call if with the evaluated results of the given if
    ;expressions condition, then, and else expressions
    (if (my-eval __cond)
      (my-eval __then)
      (my-eval __else))))

;Evaluates expressions that have expressions as their
;procedure. Ie) anonymus lambdas or let expressions that create
;and return procedures in their body.
;x -> an expression with an expression as its procedure
(define (func-expr x)
  (if (not (pair? (car x)))
    (my-eval x)
    (my-eval ((func-expr (car x)) (cdr x)))))


;LAMBDA #########################################################

;Evaluates a lambda expression
;x -> a lambda expression
;Returns a proceudeure that takes a list of arguments
(define (my-lambda x)
  (define body (check-body (car x) (cdr x)))
  ;; Create and return the new procedure
  (lambda (args)
    (let* ([__vars (make-hash)]
           [__param (car x)]
           [__body body])
      ;; Initialize all variables from the list of arguments
      ;; and push them onto the stack
      (for-each (lambda (k v)
                  (hash-set! __vars k (my-eval v)))
                __param args)
      (push __vars)
      (eval-body __body))))

;; Evaluate all expressions in the body of a lambda, let, or
;; letrec and return the result of the last one evaluated
;; after poping the local variables of the stack.
;; x -> a list of exxpressions
(define (eval-body x)
  (let ([res (map-last my-eval x)])
      (pop)
      res))

;VARIABLE CHECKING ##############################################

;; Checks a list of expressions for valid variables.
;; vars -> list of valid variables that are not on the stack yet.
;; x    -> a token or an expression
;; Returns the list with any replacements
;; Raises a ref-error if any variables are not in vars
;; or on the stack
(define (check-body vars x)
  (map (lambda (y)
         (check-expr vars y))
       x))

;; Checks an expression for valid variables.
(define (check-expr vars x)
  (if (pair? x)
    (let ([__proc (car x)]
          [__args (cdr x)])
      (cond
      [(equal? 'lambda __proc)
        (check-vars (append (cadr x) vars) x)]
      [(or (equal? 'let __proc)
           (equal? 'letrec __proc))
        (check-vars (append (map car (cadr x)) vars) x)]
      [else
        (check-vars vars x)]))
    (replace-var vars x)))

;; Checks the variables of an expression.
(define (check-vars vars x)
  (define (rec y)
    (if (list? y)
      (check-expr vars y)
      (replace-var vars y)))
  (map rec x))

;; Returns a variables value if possible, otherwise
;; returns the variable
(define (replace-var vars x)
  (if (and (symbol? x)
           (not (member? x vars)))
      (let ([v (lookup x)])
         (if (and (not (procedure? v))
                  (not (equal? v UN_INIT)))
            v
            x))
        x))

;LET/LETREC #####################################################

;; Evaluates a let expression
;; x -> a list of the arguments to a let expression
;; Returns the result of the last expression in the let body
(define (my-let x)
  (let ([__vars (make-hash)]
        [__defs (car x)]
        [__body (cdr x)])
    ;; Initalize all variable value pairs and push onto the stack
    (for-each (lambda (y)
                 (hash-set! __vars (car y) (my-eval (cadr y))))
              __defs)
    (push __vars)
    (eval-body __body)))

;; Evaluates letrec expressions
;; x -> a list of the arguments to a letrec expression
;; Returns the result of the last expression in the letrec body
(define (my-letrec x)
  (let ([__vars (make-hash)]
        [__defs (car x)]
        [__body (cdr x)])
    ;Initialize all variables to the table as UN_INIT
    (for-each (lambda (y)
                 (hash-set! __vars (car y) UN_INIT))
              __defs)
    (push __vars)
    (for-each lrec-assn __defs)
    (eval-body __body)))

;; Assigns a variable and its evaluated value to the stack.
;; x -> a variable value pair
;; Raises ref-error if the value is an uninitalized variable
(define (lrec-assn x)
  (hash-set! (car stack)
             (car x)
             (let ([__val (my-eval (cadr x))])
               ;; Don't allow values to be uninitialized vars
               (if (equal? UN_INIT __val)
                 (ref-error __val)
                 __val))))

;; HELPERS ######################################################

;; Map a given procedure onto every element in a list and return
;; the result of the last application
;; proc -> a procedure to apply
;; x    -> a list
;; Returns the result of the last application of the procedure
(define (map-last proc x)
  (letrec ([f (lambda (y res)
                (if (null? y)
                  res
                  (f (cdr y) (proc (car y)))))])
    (f x (void))))

;; Check if an element is a member of a list
;; e -> element to search for
;; x -> a list
;; Returns #t if e is a member of x, #f if not
(define (member? e x)
  (cond
  [(null? x)
    #f]
  [(equal? e (car x))
    #t]
  [else
    (member? e (cdr x))]))

;; EXTRA #########################################################
;Anything here is extra to the project description
;I just added it out of interest and for fun

(define (my-cond)
  #t)