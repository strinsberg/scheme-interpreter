#lang racket
(provide  make-env
          env-lookup
          env-push-ns
          env-add-binding
          env-empty?
          make-binding
          binding-symbol
          binding-value
          binding-eq?
          unbound?)


;; Unbound Symbols ####################################################

;; Returned when a symbol is not found in a namespace or environment
(define UNBOUND (gensym))

;; Returns true id a given value is UNBOUND, otherwise false.
;; any -> bool
(define (unbound? value) (equal? value UNBOUND))


;; Environment ########################################################

;; Returns a new environment
;; -> env
(define (make-env) (list)) 

;; Lookup a symbol in the envirionment and return its value or UNBOUND
;; symbol env -> any | unbound-symbol 
(define (env-lookup symbol env)
  (if (env-empty? env)
      UNBOUND
      (let ([value (ns-lookup symbol (env-top-ns env))])
        (if (unbound? value)
          (env-lookup symbol (env-rest env))
          value))))

;; Push an empty namespace onto the environment
;; env -> env
(define (env-push-ns env)
  (cons (make-namespace) env))

;; Add a binding onto the top namespace of an env
;; binding env -> env
(define (env-add-binding binding env)
  (if (env-empty? env)
      (env-add-binding binding (env-push-ns env))
      (cons (ns-add-binding binding (env-top-ns env)) (env-rest env))))

;; Get the top namespace in an environment
;; env -> namespace
(define (env-top-ns env) (car env))

;; Get the rest of the environment
;; env -> env
(define (env-rest env) (cdr env))

;; Check if an envirionment is empty
;; env -> bool
(define (env-empty? env)
  (null? env))


;; Namespace ##########################################################

;; Returns a new namespace
;; -> namespace
(define (make-namespace) (list))

;; Returns the value of a symbol in a namespace or UNBOUND
;; symbol namespace -> any | unbound-symbol
(define (ns-lookup symbol namespace)
  (if (null? namespace)
      UNBOUND
      (if (binding-eq? symbol (ns-top namespace))
        (binding-value (ns-top namespace))
        (ns-lookup symbol (ns-rest namespace)))))

;; Returns the top binding of the namespace
;; namespace -> binding
(define (ns-top namespace) (car namespace))

;; Returns the rest of the namespace
;; namespace -> namespace
(define (ns-rest namespace) (cdr namespace))

;; Adds a new binding to a namespace
;; namespace -> namespace
(define (ns-add-binding binding namespace)
  (cons binding namespace))


;; Binding ############################################################

;; Returns a new binding of a symbol to a value
;; -> binding
(define (make-binding symbol value) (list symbol value))

;; Returns the symbol of a binding
;; binding -> symbol
(define (binding-symbol binding) (car binding))

;; Returns the value of a binding
;; binding -> any
(define (binding-value binding) (cadr binding))

(define (binding-eq? symbol binding)
  (equal? symbol (binding-symbol binding)))

