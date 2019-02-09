#lang racket
(require "startEval.rkt")

(define failed 0)

;Testing function
;x -> a racket expression '(+ 3 4)
;expect -> the expected result
;title -> the title of the test
(define (test x expect title)
  ;(println title)
  (let ([__res (startEval x)])
    (cond
    [(not (equal? __res expect))
      (set! failed (+ failed 1))
      (display (format "~a\n" title))
      (display "!!!Failed!!!\n")
      (display (format "Expected: ~a\n" expect))
      (display (format "Actual: ~a\n" __res))
      (display "\n")])))

;Test with single data values
(test '4 4 "-- Test with a number --")
(test '"hello" "hello" "-- Test with a string --")
(test '() '() "-- Test with an empty list --")

;Test arithmetic operators
(test  '(+ (- 20 5)
                (* (/ 10 2)
                   (+ 3 1)))
        35
        "-- Test arithmetic operators --")

;Test relational operators
(test '(= 5 5) #t "-- Test = --")
(test '(< 5 3) #f "-- Test < --")
(test '(<= 3 5) #t "-- Test <= --")
(test '(> 3 5) #f "-- Test > --")
(test '(>= 5 3) #t "-- Test >= --")
(test '(equal? 12 (+ 6 6) 12 12) #t "-- Test equal? --")
(test '(equal? (quote ()) (quote ())) #t "-- Test equal? with lists --")

;Test if expressions
(test '(if (< 3 5)
          3
          5)
      3
      "-- Test if-then --")
      
(test '(if (> 3 5)
          3
          5)
      5
      "-- Test if-else --")

;Test quote
(test '(quote (+ 3 4))
      '(+ 3 4)
      "-- Test quote --")

;Test cons
(test '(cons 4 5)
      '(4 . 5)
      "-- Test cons on two numbers --")
      
(test '(cons 4 '(5 6 7))
      '(4 5 6 7)
      "-- Test with number and list --")

;Test list
(test '(list 4 5 6 7)
      '(4 5 6 7)
      "-- Test list --")

;Test car
(test '(car '(4 5 6 7))
      4
      "-- Test car --")

;Test cdr
(test '(cdr '(4 5 6 7))
      '(5 6 7)
      "-- Test cdr --")

;Test pair?
(test '(pair? '(4 5 6 7))
      #t
      "-- Test pair? on list --")

(test '(pair? 4)
      #f
      "-- Test pair? on number --")

;Test let
(test '(let ([x 4]
             [y 10])
          (let ([y 5]
                [x y])
            (+ x y)))
       15
       "-- Test let with 2 levels --")

;Test lambda
(test '((lambda (x y)
          (+ x y))
        10 20)
       30
       "-- Test lambda with following arguments --")

(test '((lambda (x y)
          (let ([z x]
                [w y])
          (+ w z)))
        10 20)
       30
       "-- Test lambda with let in body --")

(test '((lambda (f x y)
          (f x y))
        (lambda (x y) (+ x y)) 10 20)
      30
      "-- Test passing a lambda as an argument to a lambda --")

(test '((lambda (x) ((lambda (y) (+ x y)) x)) 10)
      20
      "-- Test lambda with a lambda in it's body --")

(test '(((lambda (x) (lambda (y) (+ x y))) 1) 2)
     3
    "-- Test function currying?? --")

;Test letrec
(test '(letrec ([x 5]
                [y 3])
         (+ x y))
       8
       "-- Test letrec simple --")

;Test letrec
(test '(letrec ([fact
                  (lambda (x)
                    (if (= x 0)
                      (quote 1)
                      (* x (fact (- x 1)))))])
                    (fact 10))
       3628800
       "-- Test letrec with a recursive lambda --")

;Test let rebinding keywords
(test '(let ([+ (lambda (x) (car x))]
             [let '(2 3 4 5)])
          (+ let))
        2
        "-- Test let rebinding keywords with lambda--")

(test '(let ([let '(2 3 4 5)])
          let)
        '(2 3 4 5)
        "-- Test let rebinding keywords with data --")

(test '((let ([x 4])
              (lambda (y) (+ x y)))
          5)
        9
        "-- test let expresion that returns a procedure as anonymus lambda --")

;For more good tests you should take the answers to your
;assignments questions and implement them in a way that they
;will run with your interpreter. You will just have to use letrec
;to declare all your functions instead of defining them
;all seperately. and make sure to not use racket built-ins

;Also howard is going to implement a test file for us to use

;Print test results
(if (= failed 0)
  (display "!!!All Tests Passed!!!\n")
  (display (format "Tests Failed: ~a\n" failed)))