

Behavior
========

Symbols
-------

Symbols in the program can be bound to variables/functions/macros or can be unbound. Bound symbols are evaluated to the
value they are bound to, unbound symbols are evaluated to themselves.

Consider following two short programs:

    (defvar var (+ 1 2))
    (print var)

and

    (print var)

In the first program, symbol `var` is bound to result of computation of 1 + 2. Statement (print var) will
have `var` evaluated prior to calling `print` function and output will be `3`

Second program does not have symbol `var` bound to any value, so symbol will evaluate to itself and `var` will
be printed.

**TODO Consider changing this behavior to generate exception when using unbound symbols**

