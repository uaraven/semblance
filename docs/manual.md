Semblance 2
~~~~~~~~~~~

Behavior
========

Translator
----------

Semblance is interpreted language. Reader works in two passes, first performing lexical analysis and converting input stream into list of tokens which when parsed into list of S-expressions.

Interpreter has no knowledge of special characters used in input stream, like ` `'` or `,` (single quote, backtick and comma). This characters (called read macros) are handled by parser and expanded into correct lisp forms before interpretation. For example `'expr` is expanded into (quote expr). Users of interpreter can register their handlers to process more read macros.

As all Semblance forms are JVM Serializable objects, so parsed program can be saved as standard Java Object stream. This allows to bypass translation phase for often-used programs and speed up execution.

Symbols
-------

Symbols in the program can be bound to variables/functions/macros or can be unbound. Bound symbols are evaluated to the
value they are bound to, unbound symbols are evaluated to themselves.

Consider following two short programs:

    (defvar var (+ 1 2))
    (print var)

and

    (print var)

In the first program, symbol `var` is bound to result of computation of `1 + 2`. Statement (print var) will
have `var` evaluated prior to calling `print` function and output will be `3`

Second program does not have symbol `var` bound to any value, so symbol will evaluate to itself and `var` will
be printed.

**TODO Consider changing this behavior to generate exception when using unbound symbols**


Function parameters
-------------------

Positional parameters are supported as usual. Optional parameters are supported as well

    (positional parameters &optional optional parameters)

 each optional parameter can be of following format
   - name - simplest possible case. In case parameter is omitted it will be set to NIL
   - (name default_value) - if parameter is omitted, then default_value will be supplied
   - (name default_value flag) - same as above, but flag will be set to *T* if parameter was passed to function
      and to *F* if default_value was used

 Catch-all &rest parameters are supported:

    (positional parameters &optional parameters &rest list-to-catch-all)

 All extra parameters which do not fit in positional and optional parameters will be passed as list in `list-to-catch-all`

 Keyword parameter support will be added later.
