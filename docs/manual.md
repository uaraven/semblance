Semblance 2
~~~~~~~~~~~

Behavior
========

Translator
----------

Semblance is interpreted language. Reader works in two passes, first performing lexical analysis and converting input stream into list of tokens which when parsed into list of S-expressions.

Interpreter has no knowledge of special characters used in input stream, like ` `'` or `,` (single quote, backtick and comma). This characters (called read macros) are handled by parser and expanded into correct lisp forms before interpretation. For example `'expr` is expanded into (quote expr). Users of interpreter can register their handlers to process more read macros.

As all Semblance forms are JVM Serializable objects, so parsed program can be saved as standard Java Object stream. This allows to bypass translation phase for often-used programs and speed up execution.

Semblance is *case sensitive*

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

Functions
---------

Release Functions
=================

  Semblance probably includes more special forms than Common Lisp. That's done mostly for efficiency and sometimes to avoid complex
  parameter processing. Some of the special forms may later be rewritten as functions or macros in standard library.

  **PRINTLN**

    (println value1 value2 value3 ...)

  prints all values separated by space and returns NIL

  **LET**

    (let variables body)

  where `variables` is list of (name value) definitions and `body` is sequence of S-expressions.
  Example:

    (let ((x 40)
          (y 2))
         (+ x y))

  Let is similar to Common Lisp let*, all bindings are done one by one, not in parallel. Let evaluates to last
  value in `body`

  **IF**

    (if condition then-expr else-expr?)

  Evaluates condition, if it is not equal to F executes then-expr, if condition is F and else-expr is present
  it will be executed

     (if (= a b)
         (progn ()()...)
         (println "False"))

  **QUOTE**

     (quote expression)
     or
     'expression

  Returns expression without evaluating it, for example

     (quote (+ 1 2))

  will be evaluated to `(+ 1 2)`


  **BACKQUOTE**

      (backquote expression)
      or
      `expression

  Like quote, but prevents evaluation of every symbol, except those which are un-escaped by `,` (comma)
  For example:

      `(+ ,a ,b)

  will evaluate to `(+ 1 2)` if current context has `a` bound to `1` and `b` bound to `2`.
  
  **PROGN**

    (progn (s-expression)*)

  Executes multiple expressions one by one. Result of last expression is returned

  **FN**

    (fn (parameters) (s-expression)*)

  Defines a nameless function.

  **DEFMACRO**

    (defmacro name (parameters) (s-expression)*)

  Defines a macro named `name` with a list of formal `parameters` and body of s-expressions. Just like
  other lisp out there you can use backquote to escape macro body.
  You need to use comma to un-escape parameters and you can still use `@` to unwrap lists.

    (defmacro defun
         (name params &rest body)
         `(var ,name (fn ,params ,@body)))

  Example defines a `defun` macro which takes three parameters `name`, `params` and `body` and binds to symbol
  in `name` a function with parameters `params` and `body` of s-expressions.


  **NAMESPACE**

     (namespace name (s-expression)*)

  Declares a new namespace with a name `name` in current context. Executes body and after execution binds
  all declared symbols in the new namespace.
  
  **FUNCALL**
  
    TBD
    
  **DEFUN**
  
    (defun name (params) (s-expression)*)
    
  Creates a function with parameters `params` and body defined by a list of expressions and binds it to name `name` in current context.
  Example:
    
    (defun add (x y) (+ x y))
    (add 1 2)
    
  will result in `3`
  
  **VAR**

    (var (name value) [(name value) ...])
  Binds evaluated *value* to symbol *name* in the current context. May evaluate and bind multiple variables.
  Returns latest evaluated value
