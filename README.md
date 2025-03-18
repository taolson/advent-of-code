# Advent of Code in Admiran

My solutions to all 10 years of Advent of Code, written in Admiran

## Admiran

Admiran is my own language and compiler, based upon Miranda, with various additions from Haskell and other functional languages. Features:

- pure, lazy, functional language
- self-hosting compiler written in Admiran
- whole-program optimization with inter-module inlining
- generates x86-64 assembly-language for MacOS and Linux systems
- only external dependency is the host C compiler
- small C runtime implements a 2-generation compacting garbage collector and support routines
- library, written in Admiran, implements many functional data structures, including
  - map, set, and bag collections based upon AVL balanced binary trees
  - functor/monad/applicative implementations of maybe, either, state, io
  - lens for accessing nested structures
  - mutable and immutable vectors, ST monad
  - parser combinators
