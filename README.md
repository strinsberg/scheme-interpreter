Simple Scheme Interpreter in Racket
===================================

For a project in a programming class I wrote a simple scheme interpreter using racket.

We were allowed to define simple procedures in terms of their racket equivalents, but had to write the more complicated procedures ourselves.

### Some Requirements
* We were required to use a pure functional approach so some of the structures and functions are not the most efficient.
* Expressions like let and lambda only evaluate one of the expressions in their body. Since our interpreters only implement pure functional structures there is no need to run more than one expression in the body of a let or lambda as the return value will be lost and there are no side effects allowed.
* We were only required to implement binary versions of arithmetic operations. So it is not possible to enter (+ 4 5 6 7) even though this valid lisp. My interpreter would ignore all but the first 2 elements in the previous expression.
* The goal was to be able to run any valid racket program that could be made with the subset of the language we implemented and within the given constraints. I was told that (before I tinkered with it after the semester) that I passed all the instructors tests.

### Report
We were also required to write a report on the project. It needed to outline how we approached different parts of our implementation and testing. This document is contained in ProjectReport.pdf and while I did not have a lot of time to spend on it with the end of semester crunch, I do feel that it offers some good insight into my process.

### Thoughts
This was not a super complicated project because there is only so much one can get done in a 3rd year university course in a semester. Writing the interpreter in racket meant that we did not have to parse any text because code and data are the same thing in lisps. It was also possible to make a number of the smaller elements directly with racket which made them very simple to implement. These simplifications allowed a nice introduction to the lisp language and to interpreter construction. Doing this gave me a greater understanding of the challenges of constructing and interpreting programming languages.

### Extra
The enviro.rkt module was added after the project was finished. I wanted to experiment with some simple things I read in a lisp book and try out rackets unit testing. So I made a separate module for the environment and added some unit tests. Since the environment and bindings are just lists it is all a bit unnecessary, but by using these functions one could change the implementation without requiring changes in the interpreter.

The REPL was made for fun and to do some ad-hoc testing on the interpreter. It was not required for the project and I just sort of hacked it together. It adds 2 new keywords 'define and 'expect. Define adds global definitions to the environment and expect runs an expression with the actual racket interpreter for confirmation. Since every define is just added to an environment constant defining of names would eventually take up a lot of memory (though it would take a while). The REPL can be quit with ctrl^d.

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
