Simple Lisp Interpreter in Racket
=================================

For a project in a programming class I wrote a simple lisp interpreter using racket.

We were allowed to define simple procedures in terms of their racket equivalents, but had to write the more complicated procedures ourselves.

We were also required to use a pure functional approach so some of the structures and functions are not the most efficient.

It was a really nice way to get an understanding of interpreters and functional programming without some of the complexities added by parsing text.

As an extra I added a simple REPL that can run the implemented subset of the language. It also allows global definitions.

The enviro.rkt module was added after the project was finished. I wanted to experiment with some simple things I read in a lisp book and try out rackets unit testing. So I made a separate module for the environment and added some unit tests. Since the environment and bindings are just lists it is all a bit unnecessary, but by using these functions one could change their implementation without requiring changes in the interpreter.

Try It
======

If you want to give it a try download the code and run

```
$ racket REPL.rkt
-- Welcome To My Racket REPL 1.0 --
>> (+ 4 5)
9
>> (define x 8)
>> (+ x 4)
12
```

You can also write your own lisp expressions in a file and require "startEval.rkt". Then run statEval on your code.

you-code.rkt
```
#lang racket
(require "startEval.rkt")

(define some-code '(+ 4 5))

(startEval some-code)
```

run your-code.rkt
```
$ racket your-code.rkt
9
```