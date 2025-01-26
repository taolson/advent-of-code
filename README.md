#Advent of Code in Miranda2

My solutions to all 10 years of Advent of Code, written in Miranda2

## Miranda2

Miranda2 is my own language and compiler, based upon Miranda, with various additions from Haskell and other functional languages. Features:

- pure, lazy, functional language
- self-hosting compiler written in Miranda2
- whole-program optimization with inter-module inlining
- generates x86-64 assembly-language for MacOS and Linux systems
- only external dependency is the host C compiler
- small C runtime implements a 2-generation compacting garbage collector and support routines
- library, written in Miranda2, implements many functional data structures, including
  - map, set, and bag collections based upon AVL balanced binary trees
  - functor/monad/applicative implementations of maybe, either, state, io
  - lens for accessing nested structures
  - mutable and immutable vectors, ST monad
  - parser combinators
