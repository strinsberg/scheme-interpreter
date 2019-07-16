#lang racket

(require "my-eval.rkt"
         "enviro.rkt")

(provide my-if my-quote my-lambda my-let my-letrec)


;; SIMPLE EXPRESSIONS #################################################

;; Exaluates the arguments to an if function
;; x -> a list of arguments
;; env -> an environment
;; return -> the result
(define (my-if x env)
  (let ([__cond (car x)]
        [__then (cadr x)]
        [__else (caddr x)])
    (if (my-eval __cond env)
      (my-eval __then env)
      (my-eval __else env))))

;; Evaluates the arguments to a quote function
;; x -> a list of arguments
;; env -> an environment
;; return -> the result
(define (my-quote x env)
  (quasiquote (unquote (car x))))


;; LAMBDA #############################################################
;; See report for additional documentation

;; Evaluates the arguments to a lambda function
;; x  -> a list of arguments
;; env -> an environment
(define (my-lambda x env)
  ;; Create a procedure for the lambda
  (lambda (args __env)
    (let ([__param (car x)]
          ;; Evaluate the arguments with the current environment
          [__args (map (lambda (x)
                          (my-eval x __env))
                       args)])
      ;; Evaluate the body with the environment that was current
      ;; when the lambda was created
      (my-eval (second x)
               (env-add-bindings (bindng-zip __param __args) env)))))


;; LET/LETREC #########################################################
;; See report for additional documentation

;; Evaluates the arguments to a let function
;; x -> a list of arguments
;; env -> an environment
;; return -> the result
(define (my-let x env)
  (let ([__defs (car x)])
    ;; Evaluate body after adding local bindings to the environment
    (my-eval (second x)
             (env-add-bindings (eval-bindings __defs env) env))))

;; Evaluates the arguments to a letrec function.
;; x -> a list of arguments
;; env -> an environment
;; return -> the result
(define (my-letrec x env)
  (let* ([__defs (car x)]
         ;; Add unevaluated binding to create a new environment
         [__env (append __defs env)])
    ;; Evaluate body after adding evaluated local bindings to the
    ;; environment
    (my-eval (second x)
             (env-add-bindings (eval-bindings __defs __env) env))))

