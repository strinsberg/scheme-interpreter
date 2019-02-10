#lang racket
(require "startEval.rkt")

;; Number of failed tests
(define failed 0)

;; Returns the expected value of a program or expression
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


;; Simple tests #################################################

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

;; Test lambda ##################################################

;Test lambda
(test '((lambda (x) x) 10)
       "-- Test lambda with just return x --")

(test '((lambda (x y)
          (+ x y))
        10 20)
       "-- Test lambda with following arguments --")

(test '((lambda (f x y)
          (f x y))
        (lambda (x y) (+ x y)) 10 20)
      "-- Test passing a lambda as an argument to a lambda --")

(test '((lambda (x) ((lambda (y) (+ x y)) x)) 10)
      "-- Test lambda with a lambda in it's body --")

(test '(((lambda (x) (lambda (y) (+ x y))) 1) 2)
    "-- Function that makes a function --")

;; Test let/letrec #############################################

(test '(let ([x 4]
             [y 10])
          (let ([y 5]
                [x y])
            (+ x y)))
       "-- Test let with 2 levels --")

(test '(let ([x 4]) 4 5 6 7 x)
    "-- Multiple literals in a let body --")

(test '(letrec ([x 5]
                [y 3])
         (+ x y))
       "-- Test letrec simple --")

(test '(letrec ([fact
                  (lambda (x)
                    (if (= x 0)
                      (quote 1)
                      (* x (fact (- x 1)))))])
                    (fact 10))
       "-- Test letrec with a recursive lambda (Factorial) --")

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

;; More Complex tests ##########################################

(test '((lambda (x y)
          (let ([z x]
                [w y])
          (+ w z)))
        10 20)
       "-- Test lambda with let in body --")


;; Assignment questions ########################################

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


;; Other instructor tests #######################################

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



;; Extras testing ###############################################

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


(test '(let ([f (lambda (x) (string-append "Hello, " x))])
          (f "World!"))
      "-- String Append --")

(test '(substring "corduroys" 0 4)
      "-- Substring --")

(test '(string? "Ceci n'est pas une string.")
      "-- string? #t --")

(test '(string? 1)
      "-- string? #f --")

(test '(string-length "shoelace")
      "-- string-length --")

(test '(number? 1)
      "-- number? #t --")

(test '(number? "hello")
      "-- number? #f --")

(test '(length (list 4 5 6 7 8))
      "-- length of a list --")

(test '(sqrt (* 5 5))
      "-- square root --")

(test '(not (= 4 5))
      "-- not --")
      

(test '(let ([reply-more (lambda (s)
                    (cond
                     [(equal? "hello" (substring s 0 5))
                      "hi!"]
                     [(equal? "goodbye" (substring s 0 7))
                      "bye!"]
                     [(equal? "?"
                          (substring s
                                     (- (string-length s) 1)
                                     (string-length s)))
                      "I don't know"]
                     [else "huh?"]))])
          (list (reply-more "hello racket")
                (reply-more "goodbye cruel world")
                (reply-more "what is your favorite color?")
                (reply-more "mine is lime green")))
      "-- conditional with string methods --")

(test '(let ([double (lambda (v)
                        ((if (string? v)
                            string-append
                            +)
                        v v))])
          (list (double "mnah")
                (double 5)))
      "-- conditional procedure --")

(test '(let ([twice (lambda (f v) (f (f v)))]
             [make-add-suffix (lambda (s2)
                    (lambda (s) (string-append s s2)))])
          (list (twice (make-add-suffix "!") "hello")
                (twice (make-add-suffix "?!") "hello")
                (twice (make-add-suffix "...") "hello")))
      "-- functions passing and returning functions --")

(test '(list (length (list "hop" "skip" "jump"))
             (list-ref (list "hop" "skip" "jump") 0)
             (reverse (list "hop" "skip" "jump"))
             (member "fall" (list "hop" "skip" "jump"))
             (member "jump" (list "hop" "skip" "jump")))
      "-- length, list-ref, reverse, member? --")

(test '(letrec ([my-length (lambda (lst)
                        (cond
                         [(null? lst) 0]
                         [else (+ 1 (my-length (cdr lst)))]))])
          (list (my-length (quote ()))
                (my-length (list "a" "b" "c"))))
      "-- my-length --")

(test '(letrec ([remove-dups (lambda (l)
                  (cond
                   [(null? l) (quote ())]
                   [(null? (cdr l)) l]
                   [else
                    (let ([i (car l)])
                      (if (equal? i (car (cdr l)))
                          (remove-dups (cdr l))
                          (cons i (remove-dups (cdr l)))))]))])
            (remove-dups (list "a" "b" "b" "b" "c" "c")))
      "-- remove-dups --")

(test '(list (map sqrt (list 1 4 9 16))
             (map (lambda (i)
                    (string-append i "!"))
                  (list "peanuts" "popcorn" "crackerjack"))
             (andmap string? (list "a" "b" "c"))
             (andmap string? (list "a" "b" 6))
             (ormap number? (list "a" "b" 6)))
      "-- map, andmap, ormap --")

;; Tests that we don't want to run every time
(when #f #t

;; println
(test '(println (list 1 2 3 4 5))
    "-- println --")

;; Fails most times because it uses random
(test '(let ([x (random 4)]
             [o (random 4)])
          (cond
           [(> x o) "X wins"]
           [(> o x) "O wins"]
           [else "cat's game"]))
     "-- Test random and let with cond --\n-- Will fail often as the result rely on random --")

;; Map fails because the procedure that it gets takes a list
;; of arguments rather than the one it expects
;; BREAKS FLOW
(test '(list (map sqrt (list 1 4 9 16))
             (map (lambda (i)
                    (string-append i "!"))
                  (list "peanuts" "popcorn" "crackerjack"))
             (andmap string? (list "a" "b" "c"))
             (andmap string? (list "a" "b" 6))
             (ormap number? (list "a" "b" 6)))
      "-- map, andmap, ormap --")
)


;; Print test results ###########################################
(if (= failed 0)
  (display "!!!All Tests Passed!!!\n")
  (display (format "Tests Failed: ~a\n" failed)))