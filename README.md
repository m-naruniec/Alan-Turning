# The Alan Turning programming language

## Overview

Alan Turning is an esoteric imperative programming language with a twist.

Major features:
* static typing with boolean and integer
* static binding with variable shadowing
* expression printing
* declaration and statement blocks
* nested, recursive functions
* passing function arguments by reference or by value on caller’s decision
* C-like statement expressions (eg. +=, ++, =)
* **changing direction of code execution**

The last feature is what differentiates Turning from other programming languages.
It allows programmer to decide whether their code should be executed left-to-right
(or top-down if you will).
The “turn” statement changes current direction, while left and right mark the statements as
executable only in the specified path. Even if we had not have recursive functions built in, we could
still easily mimic a while loop, making our language Turing (and Turning) complete.

## Inspiration

The language was created as a *Programming Languages and Paradigms* course semestral project.
Although it was enough to hand in a usual imperative language to pass the course, students were encouraged to implement non-standard ideas. It received maximal grade.

Turning was strongly inspired by an exercise at *Semantics and Verification of Programs* course, which introduced **left**, **right** and **turn** statements used with basic arithmetic expressions and conditionals. They needed to be extended for working in a more structured code with functions, blocks and variable declarations.

## Syntax

The syntax definition is written in [turning.cf](https://github.com/m-naruniec/Alan-Turning/blob/master/turning.cf) file using LBNF notation.

The project uses [BNFC](https://bnfc.digitalgrammars.com/) to generate lexer, parser and the skeleton of the interpreter.

## Interpreter usage

To use the interpreter, go to the root directory of cloned repository and run
    make
It will create *build* directory with *interpreter* file. You can run it with path to a source file or type your program to standard input and press ctrl+d to run it.
The interpreter will print program's output or syntax/runtime error if any occurs.

## TODO

Although the course for which it was created finished, Turning still may be developed. Here are some of the ideas:

* add more data dypes - strings, arrays, structs,
* change the main interpreting monad - there might be a better monad combination for the purpose of the language. It would probably involve Cont monad and possibly List, instead of manually written pair of continuations,
* remove recurrence from functions - this forces the use of turn, which is the whole challenge and fun in the language.



