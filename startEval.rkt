#lang racket
(provide startEval) ;Make startEval visible when required

;NOTE since we are expecting a valid racket program a large
;number of expressions that only take a certain number of
;arguments discard extra arguments rather than complaining
;about being given too many.
;ie) arithmetic operations are binary so (+ 4 5 6)
;    will return just 4 + 5 (full racket would add them all)

;NOTE in many of the rule expressions a let is used to save
;different parts of the given expression. This is not strictly
;necessary and adds overhead, but it makes the code a lot easier
;to read and know what each (car x) (caddr x) (cddr x) etc.
;is actually supposed to be.

;NOTE the checking to make sure that variables in the body of
;a lambda are valid at the time the lambda is created is
;done to make sure that lambdas do not reference variables that
;have not been declared yet. This could lead to dynamic scoping.
;Also in order to ensure that variables referenced when a lambda
;is created do not refer to other versions of those variables
;any data variables are replaced with their values when a lambda
;is created. Thus preventing dynamic scopeing. And also this
;allows functions to be return another function that uses
;parameters from the first.

;NOTE it would be better if I didn't have a list of keywords and
;3 symbol tables, but it would require changing a bunch of other
;things. This also ties into the big conditional statement that
;is in evalRec. If I could have all operators and their
;associated procedures in one table it might make that function
;a little smoother. Certainly if one wanted to add more features
;to the language set this interpreter evaluates it would be
;necessary to set things up a little differently.

;NOTE i am not sure that having startEval and evalRec is really
;necessary. evalRec is not really recursive except that it is
;called inside the functions that it calls.
;This could be useful if I were to want to put some variables on
;the stack right at the begginning. I could do it before I call
;eval rec.

;NOTE it may be possible to include the keywords in the stack at
;the top level by just having their value be their symbol instead
;of their procedure. Then as it does it can return the symbol
;so that the expression can be reevaluated with the symbol in
;place instead of the variable that points to that symbol.
;This would reduce some of the complexity of checking for
;variables in the lambda body.

;NOTE what happens when you pass too many or two few arguments
;to a lambda with your interpreter? Probably just because a
;new function is created that will try to work with a specific
;number of variables and arguments to them it will complain about
;too few. But it may just discard extras with too many? This
;probably doesn't matter so much because again we are expecting
;a valid racket program, but it is worth thinking about.

;CONSTANTS #######################################################

;A value for declared but unitialized variables
;specifically in letrec
(define UN_INIT 'uninit)
(define TEMP_PROC 'tempproc)


;SYMBOL TABLES ###################################################

;All available keywords
(define keywords (list '+ '- '* '/ 'equal? '= '<= '< '>= '> 'cdr
  'car 'cons 'pair? 'list 'lambda 'let 'letrec 'quote 'if))

;Create a procedure for unary operators that takes a list of arguments
(define (unary-op proc)
  (lambda (x)
    (proc (evalRec (car x)))))

;Create a new procedure from a binary procedure. The new procedure
;takes a list of arguments instead of 2 and will run proc on the
;evaluated results of the first 2 arguments.
;Aditional arguments will be discarded.
(define (binary-op proc)
  (lambda (x)
    (proc (evalRec (car x)) (evalRec (cadr x)))))

;Tests to see if all elements of a list are equal
;x -> a list
;Returns true if all elements in x are equal?
(define (allEqual? x)
  ;Recursivley checks if all elements of a list y equal a given
  ;element e
  (define (rec e y)
    (cond
    [(null? y)
      #t]
    [(not (equal? (evalRec e) (evalRec (car y))))
      #f]
    [else
      (rec (car y) (cdr y))]))
  ;Call the recursive function on the first element of x
  ;and the rest of x
  (rec (car x) (cdr x)))

;Rules for evaluating if expresion
;x -> an expression starting with if
;ie) (if (x) #t #f)
;Returns the result of the expression
(define (ifexpr x)
  (let ([__cond (car x)]
        [__then (cadr x)]
        [__else (caddr x)])
    ;Call if with the evaluated results of the given if
    ;expressions condition, then, and else expressions
    (if (evalRec __cond)
      (evalRec __then)
      (evalRec __else))))

;Rule for a lambda expression
;x -> a lambda expression
;ie) (lambda (x) x)
;Returns a proceudeure that takes a list of arguments
(define (lambdaexpr x)
  ;Replace all variables in the body with their values, not procedures.
  (define body (replaceVars (car x) (cdr x)))
  (checkBody (car x) body)  ;check for undeclared variables in body. If both replace and check were done in one call it could be return the body to __body in the let expression as that will evaluate when the lambda is created not when it is called later.
  ;Create a procedure to execute the body of the lambdaexp
  ;and deal with all ags and variables.
  (lambda (args)
    (let* ([__vars (make-hash)]
           [__param (car x)]
           [__body body])
      ;When the procedure is called initialize all parameters
      ;from the passed list of arguments and push the table
      ;onto the stack
      (for-each (lambda (k v)
                  (hash-set! __vars k (evalRec v)))
                __param args)
      (push __vars)
      ;Evaluates all the expressions in the body and return the
      ;result.
      (let ([res (iter evalRec __body)])
        (pop)  ;Pop local vars off the stack
        res))))

;Rule for let expressions
;x -> an expression starting with let
;ie) (let ([x 5]) (+ x 7))
;Returns the result of the last expression in the let body
(define (letexpr x)
  (let ([__vars (make-hash)]
        [__defs (car x)]
        [__body (cdr x)])
    ;For every definition pair in the let's definition section
    ;store the variable and evaluated value in the table
    (for-each (lambda (y)
                 (hash-set! __vars (car y) (evalRec (cadr y))))
              __defs)
    ;Push the table onto the stack after evaluating all the
    ;values to be stored because let does not allow assignment
    ;with variables that are being declared in it's own scope
    (push __vars)
    ;Evaluates all the expressions in the body and pops the
    ;parameter table off the stack. Then returns the result
    ;of the last evaluated expression.
    (let ([res (iter evalRec __body)])
      (pop)
      res)))

;Rule for letrec
;Allows referencing uninitialized variables
;x -> an expression starting with letrec
;ie) (letrec ([fact (lambda (x)
                      ;(if (= x 0)
                          ;(quote 1)
                          ;(* x (fact (- x 1)))))])
                    ;(fact 10))
;Returns the result of the last expression in the letrec body
(define (lrecexpr x)
  (let ([__vars (make-hash)]
        [__defs (car x)]
        [__body (cdr x)])
    ;Initialize all variables to the table as UN_INIT and push
    ;it to the stack. This allows for them to be referenced
    ;in following assignment expressions. However, it still
    ;does not allow their values to be used.
    (for-each (lambda (y)
                 (hash-set! __vars (car y) UN_INIT))
              __defs)
    (push __vars)
    ;Evaluates all the variable definitions, but doesn't allow
    ;variables in this scope to be used for assignments
    (for-each letrecAsn __defs)
    ;Same as letexpr above
    (let ([res (iter evalRec __body)])
      (pop)
      res)))

;Hash table of built-in procedures to maintain on the stack
(define keys
  (hash
    'cdr (unary-op cdr)
    'car (unary-op car)
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
    'equal? allEqual?
    'quote (lambda (x) (quasiquote (unquote (car x))))
    'list (lambda (x) (map evalRec x))
    'if ifexpr
    'lambda lambdaexpr
    'let letexpr
    'letrec lrecexpr))

;LOCAL BINDINGS ##################################################

;Stack for local variable tables
(define local '())

;Push a table onto the stack
;x -> is a hash table of variables
(define (push x)
  (set! local (cons x local)))

;Pops the top off the stack
(define (pop)
  (set! local (cdr local)))

;Looks for variable in the local variable list and returns the
;value of that variable if it is found
;Otherwise raises a error for an unbound identifier
;v -> a variable name
(define (lookup v)
  ;Recursive function to find a variable in the local stack
  ;x -> is the stack
  (define (rec x)
    (cond
    [(null? x)
      (ref-error v)]
    [(hash-has-key? (car x) v)
      (hash-ref (car x) v)]
    [else
      (rec (cdr x))]))
  ;Call the recursive function on the local variable stack
  (rec local))

;Looks for variable in the local variable list
;Returns true if the variable is found
;Otherwise false
;v -> a variable name
(define (var? v)
  ;Recursive function to find a variable in the local stack
  ;x -> is the stack
  (define (rec x)
    (cond
    [(null? x)
      #f]
    [(hash-has-key? (car x) v)
      #t]
    [else
      (rec (cdr x))]))
  ;Call the recursive function on the local variable stack
  (rec local))

;Raises an error for variables that are referenced before they
;they have been declared
;x -> the name of the variable that caused the problem
(define (ref-error x)
  (raise (format "Error: ~a: unbound identifier" x)))


;EVAL ############################################################

;Evaluates a racket program
;x -> a quoted racket program - ie) '(+ 3 (- 10 5))
;Returns the result of the program
(define (startEval x)
  (push keys)
  (evalRec x))

;Recursive function to evaluate list programs
;Calls all the functions for each kind of expression
;depending on what type of expression x is
;x -> a racket expression
;Returns the result of the expression
(define (evalRec x)
  (cond
  ;If x is a symbol if it is a keyword that hasn't been redefined
  ;just return it. Otherwise lookit up in the variable table
  ;and return it's value. lookup throws
  ;an error if x is not defined.
  [(symbol? x)
    (if (and (not (var? x)) (member x keywords))
      x
      (lookup x))]
  ;If x is a number or string or any other type of single element
  ;data then it will just be returned.
  [(not (pair? x))
    x]
  ;If x is a pair then it's first element should be a procedure
  ;And if the first element is a pair then that pair should
  ;return a procedure. So we evaluate it as such.
  [(pair? (car x))
    (funcexpr x)]
  [else
    (let ([v (lookup (car x))])
      (if (procedure? v)
        (v (cdr x))
        (raise
          (format
            "Error: expected a procedure\n  given: ~a"
            (car x)))))]))


;IF ##############################################################




;LAMBDA ##########################################################

;Recursivley Checks a list to make sure that no variables are
;referenced before they are initialized. This prevents one kind
;of dynamic scoping problem.
;args -> a list of the lambdas argument variables
;x -> a list containing all the expressions in the body
;of the lambda
(define (checkBody args x)
  (if (null? x) ;If x is empty stop
    #t
    (let ([__head (car x)]
          [__tail (cdr x)])
      (cond
      ;If x is a list check it too
      [(list? __head)
        (checkBody args __head)]
      ;If x is a symbol and it is not a keyword, variable, or
      ;argument to the lambda then raise an exception
      [(and (symbol? __head)
            (not (or
              (member __head args)
              (member __head keywords)
              (var? __head))))
        (ref-error __head)]
      ;Makes sure to add the parameters of a lambda that is
      ;defined inside x
      [(equal? __head 'lambda)
        (set! args (append (car __tail) args))]
      ;Makes sure to add the variable names of a let that is
      ;defined inside x
      [(or (equal? __head 'let)
           (equal? __head 'letrec))
        (set! args
          (append
            (map (lambda (x) (car x)) (car __tail))
            args))])
      ;Recursive call on the rest of the list
      (checkBody args __tail))))

;Goes through a list and replaces all currently assigned variables
;with their associated values.
;Needed to create and return functions in other functions
;because when the function is
;called the parameters of the function that created it are no
;longer on the stack.
;Also ensures that variables in the lambda body are bound to
;the values they have when the procedure is created and will not
;change to a different value when the function is called. Thus
;preventing another kind of dynamic scoping problem.
;Makes sure not to do this with procedures or UN_INIT variables
(define (replaceVars args x)
  (define (rec y)
    (if (list? y)
      (map rec y)
      (if (and (not (member y args))
               (var? y)  ;; guards the following lookups
               (not (procedure? (lookup y)))
               (not (equal? (lookup y) UN_INIT)))
        (lookup y)
        y)))
  (map rec x))

;Rule for evaluating expressions that have anonymous lambdas
;for their procedure. ie) '((lambda (x y) (+ x y)) 10 20)
(define (funcexpr x)
  (if (not (pair? (car x)))
    (evalRec x)
    (evalRec ((funcexpr (car x)) (cdr x)))))


;LET/LETREC ######################################################

;Assigns a variable and its evaluated value to the stack.
;Raises an error if an expression returns UN_INIT so that
;variables that have not been properly initalized yet
;cannot be used for assignment
;x -> a variable value pair
(define (letrecAsn x)
  (hash-set! (car local)
             (car x)
             ;Save the result of the evaluated value expression
             (let ([__val (evalRec (cadr x))])
               ;If it is uninitialized raise an error
               ;else return the result
               (if (equal? UN_INIT __val)
                 (ref-error __val)
                 __val))))

;Applies a function to every element of a list of expressions
;similar to for-each except it returns the result of the last
;application of f
;f -> a function to apply to all expressions
;x -> a list of expressions
;Returns the value of the last application of f
(define (iter f x)
  ;Recursive function to apply f to every element of x
  ;res -> is the result of the last expression
  ;y -> is a list of expressions
  (define (rec y res)
    (cond
    [(null? y)
      res]
    [else
      (rec (cdr y) (f (car y)))]))
  ;Call the recursive function on x with void for the result
  ;since no expression has returned a result yet
  (rec x (void)))