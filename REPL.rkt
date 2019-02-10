#lang racket
(require "startEval.rkt")

;; The global namespace for the REPL
(define namespace (make-hash))

;; Some constants and renaming
(define UN_INIT 'uninit)
(define second cadr)


;; REPL ########################################################

;; Run the REPL. Reads input from the user and prints the
;; evaluated result
(define (repl)
  (let ([__input (get-input)])      ;; Prompt and read input
    (if (or (equal? __input 'exit)  ;; Break if exit is entered
            (eof-object? __input))
        #f
        (begin
          (cond
           ;; If input is an repl procedure execute it
           [(repl-proc? __input)
              (execute (car __input) (cdr __input))]
           ;; Otherwise evaluate and print result
           [else
              (println (repl-eval __input namespace))])
          (repl)))))

;; Get input from the usee. Displays a prompt and reads all text
;; when return is pressed.
(define (get-input)
  (display ">> ")
  (with-handlers ([exn? (lambda (x) (exn-message x))])
          (read)))

;; Evaluate an expression with the racket interpreter
(define expect-eval
  (let [(ns (make-base-namespace))]
    (lambda (expr) (eval expr ns))))


;; REPL PROCEDURES #############################################

;; Checks input to see if it is an repl specific procedure
(define (repl-proc? input)
  (and (pair? input)
       (hash-has-key? global-procs (car input))))

;; Runs an named repl procedure on a list of its arguments
(define (execute name args)
      ((hash-ref global-procs name) args))

;; Runs the racket interpreter on an expression/program
;; If for some reason more than one program is given this
;; will only run the first one.
(define (expect args)
  (println (expect-eval (car args))))

;; Return a hash table of the names and procedures for all global
;; repl procedures
(define (make-global-procs)
    (hash
        'define my-define
        'expect expect))


;; DEFINE ######################################################

;; Puts the name and value of a def expression into the global ns
;; x -> the arguments to a def expression
(define (my-define x)
  (hash-set! namespace (car x) UN_INIT)
  (if (pair? (car x))
    (def-func x)
    (hash-set! namespace
               (car x)
               (repl-eval (second x) namespace))))

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


;; START REPL ###################################################

;; Runs the REPL with exception handling so that it doesn't
;; crash when you make a mistake.
(define (run)
  (with-handlers ([exn?
                      (lambda (exn)
                        (printf "~a\n" (exn-message exn))
                        (run))])
    (repl)))

;; Set up REPL specific procedures
(define global-procs (make-global-procs))

;; Display opening message
(display "-- Welcome To My Racket REPL 1.0 --\n")

;; Start the REPL
(run)