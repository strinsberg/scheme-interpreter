#lang racket/base

(require "enviro.rkt")

;; Evaluates a racket expression or program
;; x -> a racket expression or program
;; return -> the result of the evaluation
(define (my-eval x env)
  (if (my-atom? x)
    (eval-atom x env)
    (eval-function x env)


(define (eval-atom x env)
  (if (symbol? x)
    (let ([ __val (env-lookup x env)])
      (if (unbound? __val)
        (ref-error x)
        __val))
  x))


(define (eval-function x env)
  (let ([__proc (car x)]
        [__args (cdr x)])
    (cond
     [(func? __proc)
       (my-eval (cons (my-eval __proc env) __args) env)]
     [(symbol? __proc)
       (let ([__val (env-lookup __proc env)])
         (if (unbound? __val)
           (ref-error x)
           (my-eval (cons __val (__args)) env)))]
     [(proc? __proc)
       (__proc __args env)]
     [else
       (app-not-proc-error x)])))


(define (eval-bindings bindings env)
  (map (lambda (b)
          (make-binding (binding-symbol b)
                        (my-eval (binding-value b) env)))
       bindings))
