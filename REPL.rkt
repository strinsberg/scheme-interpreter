#lang racket
(require "startEval.rkt")
;; USES repl-eval

;; Renameing
(define second cadr)


;; REPL ########################################################

;; Run the REPL. Reads input from the user and prints the
;; evaluated result
(define (repl ns)
  (let ([__input (get-input)])
    (cond
     ;; EOF quits REPL
     [(eof-object? __input)
        #f]
     
     ;; REPL only functions
     [(and (pair? __input) (equal? (car __input) 'define))
        (repl (my-define (cdr __input) ns))]
     [(and (pair? __input) (equal? (car __input) 'expect))
        (println (expect-eval (second __input) ns))
        (repl ns)]
     
     ;; Evaluate and print result
     [else
        (println (repl-eval __input ns))
        (repl ns)])))

;; Get input from the usee. Displays a prompt and reads all text
;; when return is pressed.
(define (get-input)
  (display ">> ")
  (with-handlers ([exn? (lambda (x) (exn-message x))])
          (read)))

;; Evaluate an expression with the racket interpreter
(define expect-eval
  (let [(__ns (make-base-namespace))]
    (lambda (expr ns)
        (eval (cons 'let
                    (list ns
                          expr))
              __ns))))


;; DEFINE ######################################################

;; Binds a definition to a name and adds them to a namespace
;; x -> a variable or function definition
;; return -> the new namespace
(define (my-define x ns)
  (if (pair? (car x))
    (def-func x ns)
    (cons (list (car x)
                (repl-eval (second x) ns))
          ns)))

;; Binds a new function to a name and adds them to a namespace
;; x -> a function definition
;; return -> the new namespace
(define (def-func x ns)
  (let* ([__decl (car x)]
         [__name (car __decl)]
         [__args (cdr __decl)]
         [__body (cdr x)])
    (cons (list __name
                ;; Construct a lambda expression from the input
                ;; and pass it to the interpreter to get the
                ;; correct type of procedure
                (repl-eval (cons 'lambda
                                  (cons __args __body))
                           ns))
          ns)))


;; START REPL ###################################################

;; Runs the REPL with exception handling so that it doesn't
;; crash when you make a mistake.
(define (run ns)
  (with-handlers ([exn?
                      (lambda (exn)
                        (printf "~a\n" (exn-message exn))
                        (run ns))])
    (repl ns)))

;; Display opening message
(display "-- Welcome To My Racket REPL 1.0 --\n")

;; Start the REPL
(run '())