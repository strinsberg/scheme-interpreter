#lang racket
(require "startEval.rkt")

;; The global namespace for the REPL
(define namespace (make-hash))


;; REPL ###########################################################

;; Run the REPL. Reads input from the user and prints the
;; evaluated result
(define (repl)
  (let ([__input (get-input)])  ;; Prompt and read input
    (if (or (equal? __input 'exit)  ;; Break if exit is entered
            (eof-object? __input))
        #f
        (cond
         ;; If def is used store the result in the global ns
         [(and (pair? __input)
               (equal? (car __input) 'def))
            (my-define (cdr __input))
            (repl)]
         ;; Otherwise print the result and loop
         [else
            (printf "~a\n"
                    (repl-eval __input namespace))
            (repl)]))))

;; Get input from the usee. Displays a prompt and reads all text
;; when return is pressed.
(define (get-input)
  (display ">> ")
  (read))


;; DEFINE ######################################################

;; Puts the name and value of a def expression into the global ns
;; x -> the arguments to a def expression
(define (my-define x)
  (if (pair? (car x))
    (def-func x)
    (hash-set! namespace
               (car x)
               (repl-eval (cadr x) namespace))))

;; Specifically deals with defining a def expression that is
;; a procedure definition.
;; x -> the arguments to a def expression
(define (def-func x)
  (let* ([__decl (car x)]
         [__name (car __decl)]
         [__args (cdr __decl)]
         [__body (cdr x)])
    (hash-set! namespace
               __name
               ;; Construct a lambda expression from the input
               ;; and pass it to the interpreter to get the
               ;; correct type of procedure
               (repl-eval (cons 'lambda
                                (cons __args __body))
                         namespace))))


;; Start REPL #####################################################
(display "-- Welcome To My Racket REPL 1.0 --\n")
(repl)