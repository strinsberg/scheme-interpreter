#lang racket
(provide startEval)  ;; Make startEval available when required
(provide repl-eval)  ;; Make this available for REPL use

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
(define UN_INIT (gensym))
(define UNBOUND (gensym))

;; Some renaming for readability
(define second cadr)
(define third caddr)


;; EVAL #########################################################

;; Evaluates a racket program
;; x -> a racket program
;; Returns the result of the program
(define (startEval x)
  (push (builtin))  ;; Push all predefined procedures to the stack
  (my-eval x))

;; Another start to the program, but for use with an REPL that
;; has it's own namespace to pass in
(define (repl-eval x ns)
  (push (builtin))
  (push ns)
  (my-eval x))

;; Evaluates a racket expression or program
;; x -> a racket expression or program
;; Returns the result of the evaluation
(define (my-eval x)
  (cond
  ;If x is data return it or its bound value if it has one
  [(not (pair? x))
    (if (symbol? x)  ;; If it isn't a symbol skip lookup
      (let ([__val (lookup x)])
        (cond
        [(equal? __val UNBOUND)
          (ref-error 'my-eval-data x)]
        [(equal? __val UN_INIT)
          (un-init-error x)]
        [#t
          __val]))
      x)]
  ;; x is a function and its procedure is also a function
  ;; (an anonymus lambda, etc)
  [(pair? (car x))
    ((my-eval (car x)) (cdr x))]
  ;; First element of x is an actual procedure then just call
  ;; it on a list of its arguments. This really only applies to
  ;; things that are saved in function calls for later use.
  [(procedure? (car x))
    ((car x) (cdr x))]
  ;; Else x is a function and (car x) is a variable.
  [else
    (let ([__val (lookup (car x))])  ;; get variables value
      (cond
       [(equal? __val UNBOUND)  ;; variable is unbound
          (ref-error 'my-eval-procedure (car x))]
       [(procedure? __val)  ;; variable is a proceduer
          (__val (cdr x))]
       [else  ;; first element of function is not a procedure
          (raise-argument-error 'my-eval
                                "a procedure"
                                 __val)]))]))


;; BUILTINS #####################################################

;; Returns a hash table with all builtin function names and their
;; associated procedures.
(define (builtin)
  (hash
    ;; Arithmetic
    '+ (binary-op +)
    '- (binary-op -)
    '* (binary-op *)
    '/ (binary-op /)
    
    ;; Comparisson
    '= (binary-op =)
    '<= (binary-op <=)
    '< (binary-op <)
    '>= (binary-op >=)
    '> (binary-op >)
    'equal? (binary-op equal?)
    
    ;; List
    'pair? (unary-op pair?)
    'cdr (unary-op cdr)
    'car (unary-op car)
    'cons (binary-op cons)
    
    ;; Conditional
    'if my-if
    
    ;; Other
    'quote (lambda (x) (quasiquote (unquote (car x))))
    'lambda my-lambda
    'let my-let
    'letrec my-letrec
    ))

;; Redefine a given unary procedure to be a procedure that takes
;; a list of arguments and uses the first one. The new procedure
;; discards any additional arguments.
;; proc -> a racket procedure
;; Returns the new procedure.
(define (unary-op proc)
  (lambda (x)
    (proc (my-eval (car x)))))

;; Same as unary-op but the resulting procedure uses the
;; first 2 arguments.
(define (binary-op proc)
  (lambda (x)
    (proc (my-eval (car x)) (my-eval (second x)))))

;; Same as unary-op but for procedures that take 3 arguments
(define (ternary-op proc)
  (lambda (x)
    (proc (my-eval (car x))
          (my-eval (second x))
          (my-eval (third x)))))


;; VARIABLE BINDINGS #############################################

;; Stack for local variable hash tables
(define stack '())

;; Push a hash-table of variable value pairs onto the stack
;; x -> a hash table of variable names and their values
(define (push x)
  (set! stack (cons x stack)))

;; Pops the top off the stack
(define (pop)
  (set! stack (cdr stack)))

;; Looks for variable in the local variable list and returns the
;; value of that variable if it is found
;; Otherwise returns UN_INIT
;; v -> a variable name
(define (lookup v)
  (letrec ([f (lambda (x)
                (cond
                [(null? x)
                  UNBOUND]
                [(hash-has-key? (car x) v)
                  (hash-ref (car x) v)]
                [else
                  (f (cdr x))]))])
      (f stack)))

;; Raises an error for variables that are referenced before they
;; they have been declared
;; x -> the variable that caused the problem
(define (ref-error loc x)
  (raise-syntax-error x
          (string-append "undefined;\n cannot reference "
                        "an identifer before its definition")))

;; Raises an error for UN_INIT variables
;; x -> the variable that caused the problem
(define (un-init-error x)
  (raise-syntax-error x
          "undefined;\n cannot use before initialization"))


;SIMPLE EXPRESSIONS #############################################

;; Evaluates an if expresion
;; x -> a list of arguments to an if expression
;; Returns the result of applying if to the first 3 arguments
(define (my-if x)
  (let ([__cond (car x)]
        [__then (second x)]
        [__else (third x)])
    (if (my-eval __cond)
      (my-eval __then)
      (my-eval __else))))


;; LAMBDA ########################################################
;; See report for additional documentation

;; Evaluates a lambda expression
;; x -> a list of arguments to a lambda expression
;; Returns a proceudeure that takes a list of arguments
(define (my-lambda x)
  ;; Create and return the new procedure
  (lambda (args)
    (let* ([__vars (make-hash)]
           [__param (car x)]
           [__body (cdr x)])
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
      

;LET/LETREC #####################################################
;; See report for additional documentation

;; Evaluates a let expression
;; x -> a list of the arguments to a let expression
;; Returns the result of the last expression in the let body
(define (my-let x)
  (let ([__vars (make-hash)]
        [__defs (car x)]
        [__body (cdr x)])
    ;; Initalize all variable value pairs and push onto the stack
    (for-each (lambda (y)
                 (hash-set! __vars (car y) (my-eval (second y))))
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
    ;; Initialize all variables to the table as UN_INIT
    (for-each (lambda (y)
                 (hash-set! __vars (car y) UN_INIT))
              __defs)
    (push __vars)
    (for-each lrec-assn __defs)
    (eval-body __body)))

;; Assigns a variable and its evaluated value to the first level
;; of the stack. In letrec all variables are on the first level
;; they are just uninitialized so they can be refered to, but not
;; used in the assignment section.
;; x -> a variable value pair
(define (lrec-assn x)
  (hash-set! (car stack) (car x) (my-eval (second x))))


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