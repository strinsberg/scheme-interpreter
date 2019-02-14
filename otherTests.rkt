#lang racket
(require "startEval.rkt")

(define R #t)

; https://stackoverflow.com/questions/20778926/mysterious-racket-error-define-unbound-identifier-also-no-app-syntax-trans
(define expectEval
  (let [(ns (make-base-namespace))]
    (lambda (expr) (eval expr ns))))

; testing function.
(define (test name xpr)
  (let [(expect (expectEval xpr))]
    (when R (printf "~a | " (~a expect #:width 12)))
    (let [(actual (startEval xpr))]
      (when R (printf "got ~a | ~a " (~a actual #:width (if #t 16 66)) (~a name #:width 22)))
      (if (equal? expect actual)
          (if R (printf "  (pass)  ") (printf "---------> pass\n\n\n"))
          (if R (printf " (*FAIL*) ") (printf "---------> FAIL: got ~a | expected ~a\n\n\n" (~a actual) (~a expect))))
      (when R (printf "~a|\n" (~a xpr #:width 32))))))


(when #t #t ; passing tests.
  (test "const"
        '100)
  (test "const list"
        '(list 1 2 3))
  (test "dynamic list"
        '(list 1 (+ 1 1) 3))

  (test "addition"
        '(+ 100 23))
  (test "subtraction"
        '(- 100 23))
  (test "multiplication"
        '(* 100 23))
  (test "division"
        '(/ 100 23))

  (test "nested math"
        '(+ 100 (+ 20 3)))

  (test "relational ="
        '(= 123 456))
  (test "relational <="
        '(<= 123 456))
  (test "relational <"
        '(< 123 456))
  (test "relational >="
        '(>= 123 456))
  (test "relational >"
        '(> 123 456))
  (test "relational equal?"
        '(equal? 123 456))

  (test "quote verbose"
        '(quote (+ 100 23)))
  (test "quote char"
        ''(+ 100 23))

  (test "if"
        '(if #t 123 456))
  (test "if"
        '(if #f 123 456))
  (test "if"
        '(if (if #t #f #f) 123 456))

  (test "lists car"
        '(car (cons 1 2)))
  (test "lists cdr"
        '(cdr (cons 1 2)))
  (test "lists cons"
        '(cons 1 2))
  (test "lists pair?"
        '(pair? (cons (+ 0 1) 2)))
  (test "lists cdr quote"
        '(cdr '(1 2)))

  (test "let1"
        '(let [(x 1)]
           x))
  (test "let2"
        '(let [(x (+ 1 1))]
           x))
  (test "let3"
        '(let [(x 1) (y 2)]
           (+ x y)))
  (test "let4"
        '(let [(x 1)]
           (let [(y 3)]
             (+ x y))))
  (test "let5a"
        '(let [(x 5)]
           (let [(x -1)]
             x)))
  (test "let5b"
        '(let [(x -1)]
           (let [(x 5)]
             x)))
  (test "let6"
        '(let [(x '(+ 1 5))]
           x))
  (test "let7"
        '(let [(x 1)]
           (list x (+ x x))))
  (test "let8"
        '(let [(f (lambda () (+ 1 2)))]
           (f)))
  (test "let9"
        '(let [(f (lambda (a b c) (+ a (+ b c))))]
           (f 1 2 3)))
  (test "letrec"
        '(letrec [(f (lambda () (+ 1 2)))]
           (f)))
  (test "override global"
        '(letrec [(x 1) (f (lambda (a) (+ x a)))]
           (let [(x 1000) (y 2)]
             (f y))))
  (test "override local"
        '(letrec [(x 1) (f (lambda (x) (+ x x)))]
           (f 2)))
  (test "function ambiguity 1"
        '(let [(f 0)]
           (let [(f (lambda () (+ 1 2)))]
             (f))))
  (test "function ambiguity 2"
        '(let [(f (lambda () (+ 1 2)))]
           (let [(f 0)]
             f)))
  (test "override vanilla"
        '(let [(x 1)]
           (let [(f (lambda () (+ x 10)))]
             (letrec [(x 3)]
               (f)))))
  (test "adder"
        '(letrec [(adder (lambda (x)
                           (if (<= x 0)
                               0
                               (+ x (adder (- x 1))))))]
           (adder 3)))
  
  (test "factorial"
        '(letrec [(fact
                   (lambda (x)
                     (if (= x 0) (quote 1)
                         (* x (fact (- x 1))))))]
           (fact 10)))
  (test "sum of list"
        '(letrec [(listsum (lambda (x) (if (pair? x) (+ (car x) (listsum (cdr x))) 0)))]
           (listsum (quote (1 2)))))
  (test "let return list"
        '(let [(f (lambda (x) x))] (f (quote (1 2)))))
  (test "letrec return list"
        '(letrec [(f (lambda (x) x))] (f (quote (1 2)))))
  (test "max of list"
        '(letrec [(max (lambda (x)
                         (if (pair? (cdr x))
                             (if (> (car x) (car (cdr x)))
                                 (car x)
                                 (max (cdr x)))
                             (car x))))]
           (max '(1 20 70 70 54 7))))
  (test "count list"
        '(letrec [(countList (lambda (x)
                               (if (> x 0)
                                   (cons x (countList (- x 1)))
                                   '())))]
           (countList 10)))
  (test "fibonacci"
        '(letrec [(fib (lambda (x y z)
                         (if (< 0 z)
                             (cons x (fib y (+ x y) (- z 1)))
                             '())))]
           (fib 1 1 5)))
  (test "power"
        '(letrec [(pow (lambda (x y)
                         (if (= y 0)
                             1
                             (* x (pow x (- y 1))))))]
           (pow 2 5)))
  (test "harmonic series"
        '(letrec [(harmonicSeries (lambda (x y)
                                    (if (= y 0)
                                        0
                                        (+ x (harmonicSeries (/ x 2) (- y 1))))))]
           (harmonicSeries 1 10)))

  (test "pi computation"
        '(letrec [(Nilakantha (lambda (x y z w)
                                (if (= w 0)
                                    3
                                    (+ (- (/ 4 (* x (* y z))) (/ 4 (* (+ x 2) (* (+ y 2) (+ z 2))))) (Nilakantha (+ x 4) (+ y 4) (+ z 4) (- w 1))))))]
           (Nilakantha 2 3 4 10)))
  (test "cat override"
        '(letrec [(cat (lambda (l)
                         (if (equal? (quote ()) l)
                             0
                             (+ (car l) (cat (cdr l))))))]
           (cat '(2 3 4))))
  (test "crazy"
        '(letrec [(f1 (lambda (a) (+ a a))) (f2 (lambda (a) (* a a))) (f3 (lambda (a b) (f1 (+ b (f2 a)))))]
           (f3 1 2)))
  (test "varbasic"
        '(letrec [(f (lambda (x) x))]
           (f '())))
  
  (test "varlist"
        '(letrec [(f (lambda (x) x))]
           (f (quote (1 2)))))

  (test "varlistrec"
        '(letrec [(f (lambda (x) x))]
           (f (list 0 (list 1) 2 3))))
  
  (test "varlistrec"
        '(letrec [(f (lambda (x) x))]
           (f (list 0 (list (list (list 1 2) 3 4 5))))))
  
  (test "metalist"
        '(letrec [(f (lambda () (list 0 (list (list 1 2) 3 4 5 (list 6 7 8))))) (f2 (lambda (x) x))]
           (f2 (f))))

  (test "quote override" '(letrec [(quote (lambda (a) (+ a 1)))] (quote 1)))
  (test "null? override" '(letrec [(null? (lambda (a) (+ a 1)))] (null? 1)))
  (test "pair? override" '(letrec [(pair? (lambda (a) (+ a 1)))] (pair? 1)))
  (test "car override" '(letrec [(car (lambda (a) (+ a 1)))] (car 1)))
  (test "cdr override" '(letrec [(cdr (lambda (a) (+ a 1)))] (cdr 1)))

  (test "cons override" '(letrec [(cons (lambda (a b) (+ a b)))] (cons 1 2)))

  (test "+ override" '(letrec [(+ (lambda (a b) (/ a b)))] (+ 1 2)))
  (test "- override" '(letrec [(- (lambda (a b) (+ a b)))] (- 1 2)))
  (test "* override" '(letrec [(* (lambda (a b) (+ a b)))] (* 1 2)))
  (test "/ override" '(letrec [(/ (lambda (a b) (+ a b)))] (/ 1 2)))

  (test "equal? override" '(letrec [(equal? (lambda (a b) (+ a b)))] (equal? 1 2)))
  (test "= override" '(letrec [(= (lambda (a b) (+ a b)))] (= 1 2)))
  (test "<= override" '(letrec [(<= (lambda (a b) (+ a b)))] (<= 1 2)))
  (test "< override" '(letrec [(< (lambda (a b) (+ a b)))] (< 1 2)))
  (test ">= override" '(letrec [(>= (lambda (a b) (+ a b)))] (>= 1 2)))
  (test "> override" '(letrec [(> (lambda (a b) (+ a b)))] (> 1 2)))

  (test "super lambda" '(let [(l (lambda (a b) (+ a b)))] (let [(f l)] (f 1 2))))
  (test "super lambda2" '(let [(r (lambda (a b) (+ a b)))] (let [(l r)] (let [(f l)] (f 1 2)))))
  (test "super lambda override" '(let [(r (lambda (a b) (+ a b)))] (let [(l r)] (let [(lambda l)] (lambda 1 2)))))
  
  (test "lambda override" '(let [(l (lambda (a b) (+ a b)))] (letrec [(lambda l)] (lambda 1 2))))
  (test "let override" '(letrec [(let (lambda (a b) (+ a b)))] (let 1 2)))
  (test "letrec override" '(let [(letrec (lambda (a b) (+ a b)))] (letrec 1 2)))

  (test "forever override if"
        '(letrec [(f (lambda (x)
                       (if (= x 0)
                           #t
                           (f (- x 1)))))]
           (f 0)))

  (test "if override" '(letrec [(if (lambda (c a b) (+ c (+ a b))))] (if 1 2 3)))
  (test "if override2"
        '(letrec [(if (lambda (a b c)
                        (+ a (+ b c))))]
           (if 3 8 5)))

  (test "failstar" '(letrec [(l (lambda (a b) (+ a b))) (r l)] (r 1 2)))
  (test "failstarstack" '(letrec [(l (lambda (a b) (+ a b))) (r l) (a 1) (b 1) (c 1) (d 1) (e 1) (f 1)] (r 1 2)))
  (test "stress" '(letrec [(az 10) (azw 10) (azwz 10) (awzz 10) (a (lambda (a b) (+ a b))) (z 10) (zw 10) (zwz 10) (wzz 10) (b a) (zs 10) (zws 10) (zsz 10) (c b) (zzs 10) (zwzs 10) (equal? c)] (equal? 1 2)))
  (test "superrecursive" '(letrec [(l (lambda (a b) (+ a b))) (r (lambda (a b) (+ a (l a b)))) (za 100) (zb 100) (zc 100) (zd 100) (let r) (zq 100) (zw 100) (ze 100) (zr 100)] (let 1 2)))
  
  (test "superquotes" '(list '+ ''''- '(+ * -) '/)) ; '(+ - * /)
  
  (test "howards final showdown" '(let [(let (lambda (x) (cdr x))) (letrec '(1 2 3 4 5))] (let letrec)))

  (test "dynamic null?" '(letrec [(f null?)] (null? '())))
  (test "dynamic pair?" '(letrec [(f pair?)] (pair? '())))
  (test "dynamic car" '(letrec [(f car)] (f '(1 2 3))))
  (test "dynamic cdr" '(letrec [(f cdr)] (f '(1 2 3))))

  (test "dynamic cons" '(letrec [(f cons)] (f '(1 2 3) '(4 5 6))))

  (test "dynamic +" '(letrec [(f +)] (f 1 2)))
  (test "dynamic -" '(letrec [(f -)] (f 1 2)))
  (test "dynamic *" '(letrec [(f *)] (f 1 2)))
  (test "dynamic /" '(letrec [(f /)] (f 1 2)))

  (test "dynamic equal?" '(letrec [(f equal?)] (f 1 2)))
  (test "dynamic =" '(letrec [(f =)] (f 1 2)))
  (test "dynamic <=" '(letrec [(f <=)] (f 1 2)))
  (test "dynamic <" '(letrec [(f <)] (f 1 2)))
  (test "dynamic >=" '(letrec [(f >=)] (f 1 2)))
  (test "dynamic >" '(letrec [(f >)] (f 1 2)))
  )