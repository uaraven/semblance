# Semblance Interpreter internals

This document describes some implementation details of Semblance v2 implementation, in particular the way the code
is evaluated.

## Difference between Interpreter and Context

`Interpreter` is a helper class which combines `Reader` and `Context` (particularly `RootContext`) and is a 
convenient tool to parse and execute a program. 

`Context` is an interface for a program execution. It defines a program state in any given moment of time. `Context`
evaluates expressions, maintains namespaces and variable bindings. Contexts can be nested and context will look up
its parent to find bindings.

There are two main types of `Context` in Semblance implementation: `RootContext` and `LocalContext`. `RootContext` 
binds all the built-in functions and special forms. It also loads and executes standard library ("semblance.smbl" file).
Essentially `RootContext` *IS* Semblance.
 
`LocalContext` is created during program execution as it is necessary. Every program construct which defines a logical
block does so using `LocalContext`. `(progn ...)` and `(block ...)` and `(let ...)` and some special forms, like 
`(loop ...)` all define local contexts.
 
`LocalContext` is what allows to have local variables in `(let)` block or a function. `LocalContext` can be named or
nameless, usually functions name their local contexts after themselves, other statements may leave context unnamed.

Semblance does not really have a concept of stack frames, but `LocalContext` is as close as you can get to it.


## Code evaluation

### Expression evaluation

Code execution magic happens inside `Context.evaluate()` method. Algorithm to evaluate any expression is as follows:
 
  - if expression is a symbol
    - if it is a keyword then return expression itself as a result, keywords always evaluate to themselves
    - if symbol is bound to a value, then return the value
    - if symbol is unbound throw `UnboundSymbolExpression`
  - if expression is a list:
    - if it is a NIL list, return it as result
    - otherwise evaluate list as a function (see `Context.evaluateFunction()`)
  - if expression is a map literal:
    evaluate map literal. Map literals (`{ key value key value ....}`) are evaluated when they are executed, each 
    expression inside literal is evaluated, even expressions are used as keys, and odd expressions are treated as values.
    Map of keys and values is returned as a result of literal evaluation.
  - if expression is a vector literal:
    evaluate vector literal. Vector literals (`[expression expression ...]`) are evaluated when they are executed, each
    value inside literal is evaluated and evaluated vector is returned as a result of literal evaluation.
  - otherwise return expression as is
  
Notes:

Vector and Map literal evaluation is performed in the same context where they are defined, no new context is created.
List literals are treated differently than vectors and maps as lists are building blocks of Semblance. Each list literal
is treated as function call. To define a list either use `list` function or `quote` reader macro.
  
### Function evaluation

Function evaluation is perfomed inside `Context.evaluateFunction()` method. `evaluateFunction()` accepts `SList` containing
a function call as a parameter.

First head of function (which is a list, remember?) is checked to validate if it can be called. 

Function call list is a valid function call if and only if its head is either a callable or a symbol bound to callable.

After list head is resolved to callable `LispCallable.apply()` is called with *unevaluated* parameters passed to it.
No new context is created for the `apply()` call either. Parameter evaluation and context creation is up to callable 
implementation. 

Callables can be divided into three broad categories:
  - functions
  - special forms
  - macros
  
**Functions** first create new named context and then evaluate their actual parameters inside that context, binding
evaluated values to formal parameter names. 
 
From inside function parameters look exactly like usual local variables.

**Special forms** do not evaluate their parameters, they pass them to code as is. This allows code to be passed as parameter
and allows manipulation of mentioned code. Special forms are part of Semblance implementation.

**Macros** are somewhat like special forms, but can be defined in Semblance code and have some of their parameters evaluated
and some passed as-is.