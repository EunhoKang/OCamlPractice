## Goal
The goal of this homework is to implement two interpreters for the language with recursive procedures and explicit references (one with static scoping and the other with dynamic scoping).  
The syntax and semantics of the language are defined in "hw4.pdf".

## Specification
- Implement the ***eval*** functions in the "interpreter_dynamic.ml" and "interpreter_static.ml" files.
- Do not modify the types and names of items in the "lang.ml" except for the ***eval*** function.
- Use the OCaml's *read_int* function for implementing the *read* expression:
  - http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
- You can use the environment and memory implementation for the interpreter which are provided as built-in.
- During execution, raise the exception *UndefSemantics* whenever the semantics is undefined.
  - e.g.) ADD expressions with a value of Bool type, a conditional expression of type int, etc 
  - You should use the pre-defined exception that we provided in "lang.ml".
  - You can raise the exception by ```raise UndefinedSemantics```.

## Compilation and Execution
Compile and execute the interpreter as follows:
```
  make clean; make             (* for compilation *)
  ./run -pp test_dynamic/proc1.m       (* for checking how the program is represented by the ocaml datatype *)
  ./run -dynamic test_dynamic/proc1.m  (* running the interpreter with dynamic scoping *)
  ./run -static test_static/proc1.m   (* running the interpreter with static scoping *)
```

## Correctness 
Your program should behave as described in ***test_log_dynamic*** and ***test_log_static***. 

## How to Submit
Submit "interpreter_dynamic.ml" for Exercise 1 and "interpreter_static.ml" for Exercise 2. 
The files should be compilable. 
