#lang racket/base

(require "enviro.rkt"
         "my-procedures")

(provide environment)

;; The top level environment
(define environment
  (env-add-bindings bindings (make-env)))


;; List of top level bindings #########################################

(define bindings
  (list
    ;; Arithmetic
    (make-binding '+ (binary-op +))
    (make-binding '- (binary-op -))
    (make-binding '* (binary-op *))
    (make-binding '/ (binary-op /))
    
    ;; Comparisson
    (make-binding '= (binary-op =))
    (make-binding '<= (binary-op <=))
    (make-binding '< (binary-op <))
    (make-binding '>= (binary-op >=))
    (make-binding '> (binary-op >))
    (make-binding 'equal? (binary-op equal?))
    
    ;; List
    (make-binding 'pair? (unary-op pair?))
    (make-binding 'cdr (unary-op cdr))
    (make-binding 'car (unary-op car))
    (make-binding 'cons (binary-op cons))
    
    ;; Conditional
    (make-binding 'if my-if)
    
    ;; Other
    (make-binding 'quote my-quote)
    (make-binding 'lambda my-lambda)
    (make-binding 'let my-let)
    (make-binding 'letrec my-letrec)
    ))


;; Builtin procedure wrappers #########################################

;; Redefine a given unary procedure to take a list of arguments
;; and a namespace. New procedure calls proc on the first arg.
;; procedure<any> -> procedure<arg list, environment> 
(define (unary-op proc)
  (lambda (x ns)
    (proc (my-eval (car x) ns))))

;; Same as unary-op but for binary procedures
;; procedure<any, any> -> procedure<arg list, environment> 
(define (binary-op proc)
  (lambda (x ns)
    (proc (my-eval (car x) ns)
          (my-eval (second x) ns))))

;; Same as unary-op but for ternary procedures
;; procedure<any, any, any> -> procedure<arg list, environment> 
(define (ternary-op proc)
  (lambda (x ns)
    (proc (my-eval (car x) ns)
          (my-eval (second x) ns)
          (my-eval (third x) ns))))

