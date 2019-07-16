#lang racket
(provide  make-env
          env-lookup
          env-add-binding
          env-add-bindings
          env-empty?
          make-binding
          binding-symbol
          binding-value
          binding-eq?
          binding-zip
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
    (if (binding-eq? symbol (env-car env))
      (binding-value (env-car env))
      (env-lookup symbol (env-rest env)))))

;; Add a binding onto the top namespace of an env
;; binding env -> env
(define (env-add-binding binding env)
  (cons binding env))

;; Add a list of bindings to the given environment
;; list of binding env -> env
(define (env-add-bindings bindings env)
  (if (null? bindings)
    env
    (env-add-bindings (cdr bindings)
                      (cons (car bindings) env))))

;; env -> binding
(define (env-car env) (car env))

;; Get the rest of the environment
;; env -> env
(define (env-rest env) (cdr env))

;; Check if an envirionment is empty
;; env -> bool
(define (env-empty? env)
  (null? env))


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

;; Checks to see if a binding is for the given symbol
;; symbol binding -> bool
(define (binding-eq? symbol binding)
  (equal? symbol (binding-symbol binding)))

;; Takes a list of symbols and a list of values and returns a list
;; of bindings with each symbol associated with the value at the same
;; position in the other list.
;; list list -> list of binding
(define (binding-zip symbols vals)
  (map make-binding symbols vals))

