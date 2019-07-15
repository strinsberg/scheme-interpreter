#lang racket/base

(require rackunit
         rackunit/text-ui
         "enviro.rkt")

(define binding-tests
  (test-suite
    "Tests the binding structure"

    (let ([bind (make-binding 'x 5)])
        
      (check-equal? (binding-symbol bind) 'x "get symbol")
      (check-equal? (binding-value bind) 5 "get value"))))
    

(define env-tests
  (test-suite
    "Tests the environment structure."

    (let ([env (make-env)]
          [bind_x (make-binding 'x 5)]
          [bind_y (make-binding 'y 10)])
     (check-equal? env '() "make-env")

     (test-case
       "Test env-empty"

       (check-equal? (env-empty? env) #t "new env")
       (check-equal? (env-empty? (env-add-binding bind_x env)) #f "not empty"))
     
     (check-equal? (env-add-binding bind_x env) '(((x 5))) "add binding") 

     (test-case
       "Test env-lookup"
     
       (check-equal? (unbound? (env-lookup 'x env)) #t "unbound symbol")
       (check-equal? (env-lookup 'x
                                 (env-add-binding bind_x env))
                     5
                     "lookup var in top level")
       (check-equal? (env-lookup 'x
                                 (env-add-binding bind_y
                                                  (env-add-binding bind_x env)))
                     5
                     "lookup with more than one value in top level")
       (check-equal? (env-lookup 'x
                                 (env-add-binding
                                   bind_y
                                   (env-push-ns (env-add-binding bind_x env))))
                     5
                     "lookup with more than one level var not in top"))
     )))

(run-tests binding-tests 'verbose) 
(run-tests env-tests 'verbose)

