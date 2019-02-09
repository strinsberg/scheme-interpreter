#lang racket
(require "startEval.rkt")

(define failed 0)

(define expect-eval
  (let [(ns (make-base-namespace))]
    (lambda (expr) (eval expr ns))))

;Testing function
;x -> a racket expression '(+ 3 4)
;expect -> the expected result
;title -> the title of the test
(define (test x title)
  (let ([__expected (expect-eval x)]
        [__actual (startEval x)])
    (cond
    [(not (equal? __expected __actual))
      (set! failed (+ failed 1))
      (display (format "~a\n" title))
      (display "!!!Failed!!!\n")
      (display (format "Expected: ~a\n" __expected))
      (display (format "Actual: ~a\n" __actual))
      (display "\n")])))

;Test with single data values
(test '4 "-- Test with a number --")
(test '"hello" "-- Test with a string --")
(test '#t "-- Test with an empty list --")

;Test arithmetic operators
(test  '(+ (- 20 5)
           (* (/ 10 2)
             (+ 3 1)))
        "-- Test arithmetic operators --")

;Test relational operators
(test '(= 5 5) "-- Test = --")
(test '(< 5 3) "-- Test < --")
(test '(<= 3 5) "-- Test <= --")
(test '(> 3 5) "-- Test > --")
(test '(>= 5 3) "-- Test >= --")
(test '(equal? 12 (+ 6 6)) "-- Test equal? --")
(test '(equal? (quote ()) (quote ()))
      "-- Test equal? with lists --")

;Test if expressions
(test '(if (< 3 5)
          3
          5)
      "-- Test if-then --")
      
(test '(if (> 3 5)
          3
          5)
      "-- Test if-else --")

;Test quote
(test '(quote (+ 3 4))
      "-- Test quote --")

;Test cons
(test '(cons 4 5)
      "-- Test cons on two numbers --")
      
(test '(cons 4 '(5 6 7))
      "-- Test with number and list --")

;Test list
(test '(list 4 5 6 7)
      "-- Test list --")

;Test car
(test '(car '(4 5 6 7))
      "-- Test car --")

;Test cdr
(test '(cdr '(4 5 6 7))
      "-- Test cdr --")

;Test pair?
(test '(pair? '(4 5 6 7))
      "-- Test pair? on list --")

(test '(pair? 4)
      "-- Test pair? on number --")

;Test let
(test '(let ([x 4]
             [y 10])
          (let ([y 5]
                [x y])
            (+ x y)))
       "-- Test let with 2 levels --")

;Test lambda
(test '((lambda (x) x) 10)
       "-- Test lambda with just return x --")

(test '((lambda (x y)
          (+ x y))
        10 20)
       "-- Test lambda with following arguments --")

(test '((lambda (x y)
          (let ([z x]
                [w y])
          (+ w z)))
        10 20)
       "-- Test lambda with let in body --")

(test '((lambda (f x y)
          (f x y))
        (lambda (x y) (+ x y)) 10 20)
      "-- Test passing a lambda as an argument to a lambda --")

(test '((lambda (x) ((lambda (y) (+ x y)) x)) 10)
      "-- Test lambda with a lambda in it's body --")

(test '(((lambda (x) (lambda (y) (+ x y))) 1) 2)
    "-- Test function currying?? --")

;Test letrec
(test '(letrec ([x 5]
                [y 3])
         (+ x y))
       "-- Test letrec simple --")

;Test letrec
(test '(letrec ([fact
                  (lambda (x)
                    (if (= x 0)
                      (quote 1)
                      (* x (fact (- x 1)))))])
                    (fact 10))
       "-- Test letrec with a recursive lambda (Factorial) --")

;Test let rebinding keywords
(test '(let ([+ (lambda (x) (car x))]
             [let '(2 3 4 5)])
          (+ let))
        "-- Test let rebinding keywords with lambda--")

(test '(let ([let '(2 3 4 5)])
          let)
        "-- Test let rebinding keywords with data --")

(test '((let ([x 4])
              (lambda (y) (+ x y)))
          5)
        "-- test let expresion that returns a procedure as anonymus lambda --")

;Assignment questions
(test '(letrec ([levRec (lambda (n x cur res)
                          (if (equal? x (quote ()))
                            res
                            (if (= n cur)
                              (cons (car x) res)
                              (levList n (cdr x) (+ cur 1) res))))]
                [levList (lambda (n lx cur res)
                          (if (equal? lx (quote ()))
                            res
                            (levRec n (car lx) cur (levList n (cdr lx) cur res))))])
                (levRec
                  3
                  (quote (1 (2 (5 () ()) (6 ())) (3 ()) (4 ())))
                  1
                  (quote ())))
        "-- Getting all labels at a tree level --")

(test '(letrec ([greater (lambda (a b)
                          (if (> a b) a b))]
              [treeH (lambda (h x)
                      (if (equal? x (quote ()))
                        h
                        (childrenH (+ h 1) (cdr x))))]
              [childrenH (lambda (h lx)
                          (if (equal? lx (quote ()))
                            h
                            (greater
                              (treeH h (car lx))
                              (childrenH h (cdr lx)))))])
              (treeH
                0
                (quote (1
                          (2 (3 () ()) (4 () ((6 ()))))
                          (15 () ())
                          (16 () ())))))
        "-- Finding the height of a tree --")

;; test cond
(test '(letrec ([member? (lambda (e x)
                            (cond
                            [(null? x)
                              #f]
                            [(equal? e (car x))
                              #t]
                            [#t
                              (member? e (cdr x))]))])
              (member? 4 (list 3 6 5 2 4 6)))
      "-- Test cond with a member function --")

;; Other instructor tests
(test '(let ([inc (lambda (x) (+ x (quote 1)))])
        (inc (quote 5)))
      "-- Instructor Test: lambda in let with quotes --")

(test '(letrec ((fib
            (lambda (n) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))
           
           (fib 7))
      "-- Instructor Test: Fibonacci --")

(test '(let ([sub1 (lambda (x) (- x 1))]
                  [not (lambda (x) (if x #f #t))])
                  
              
              (letrec ([is-even? (lambda (n)
                                   (if (= n '0)
                                       #t
                                       (is-odd? (sub1 n))))]
                       [is-odd? (lambda (n)
                                  (if (not (= n '0))
                                      (is-even? (sub1 n))
                                      '#f
                                      ))])
                (is-odd? 11)))
      "-- Instructor Test: Mutally recursive functions --")

(test '(letrec ((intersect
             (lambda (s t)
               (if (equal? s (quote ()))
                 (quote ())
                 (if (member (car s) t)
                   (cons (car s) (intersect (cdr s) t))
                   (intersect (cdr s) t)
                 )
               )
              ))
             (member
              (lambda (x s)
                 (if (equal? s (quote ()))
                   (quote #f)
                   (if (equal? x (car s))
                     (quote #t)
                     (member x (cdr s))
                   )
                 )
              )
             ))
           (intersect (quote (a b c d)) (quote (b c d e f))))
      "-- Instructor Tests: Intersect --")


;Print test results
(if (= failed 0)
  (display "!!!All Tests Passed!!!\n")
  (display (format "Tests Failed: ~a\n" failed)))