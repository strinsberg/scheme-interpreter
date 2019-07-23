#lang racket/base

(require rackunit
         rackunit/text-ui
         "enviro.rkt")

;; Tests for the environment module
;; My first try at doing unit testing with racket
;; Hopefully the tests names are sufficient to document the tests.
;; The implementation of the environment and bindings is not complex.
(define env-tests
  (test-suite
    "test envrionment module"
    
    (let ([env (make-env)]
          [bind_x (make-binding 'x 5)]
          [bind_y (make-binding 'y 10)])

      (test-case
        "Tests the binding structure"
        
        (check-equal? (binding-symbol bind_x) 'x "get symbol")
        (check-equal? (binding-value bind_x) 5 "get value"))

      (test-case
        "Test env-empty"

        (check-equal? (env-empty? env) #t "new env")
        (check-equal? (env-empty? (env-add-binding bind_x env)) #f "not empty"))
     
      (test-case
        "Test env-lookup and binding"
     
        (check-equal? (unbound? (env-lookup 'x env))
                      #t
                      "lookup unbound env emtpy")
        (check-equal? (unbound? (env-lookup 'x
                                            (env-add-binding bind_y env)))
                      #t
                      "lookup unbound env not empty")
        (check-equal? (env-lookup 'x
                                  (env-add-binding bind_x env))
                      5
                      "lookup var in top level")
        (check-equal? (env-lookup 'x
                                  (env-add-binding bind_y
                                                   (env-add-binding bind_x
                                                                    env)))
                      5
                      "lookup with more than one value in top level")
        (check-equal? (env-lookup 'x
                                  (env-add-binding
                                    (make-binding 'x 10)
                                    (env-add-binding bind_x env)))
                      10
                      "lookup variable re-declared"))

      (test-case
        "Test binding zip"

        (let ([symbols (list 'x 'y 'z)]
              [vals (list 5 10 15)])
          (check-equal? (env-lookup 'z
                                    (env-add-bindings
                                      (binding-zip symbols vals)
                                      env))
                        15))) 
      )))

(run-tests env-tests 'verbose)

