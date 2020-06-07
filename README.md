# README #

An implementation of a simple regex engine, to explore finite automata implementation.

Built with Ada.

## Features

- Support for most basic regex features:
    - Characters
    - Ranges
    - Range Complements
    - Unions
    - Concatenation
    - `?`, `*`, and `+` operators
    - Certain whitespace escape sequences: `\t`, `\n`
- Implemented with 
    - Recursive descent parsing with simple backtracking
    - Conversion of regex to an NFA, and then to a DFA

## Major Dependencies

- AUnit for testing
- GNAT Community 2019 Ada Compiler

## Build/Deploy Instructions

The simplest way to run the tests for this implementation is to use 
the GNAT Pro Studio IDE from AdaCore, which manages the build process.
Just use `Build > Project > Build & Run > main.adb`

## Contact

Questions/comments can be sent to <hchen7913@gmail.com>