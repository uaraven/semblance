Semblance 2
===========

Behavior
--------

### Translator


Semblance is interpreted language. Reader works in two passes, first performing lexical analysis and converting input
stream into list of tokens which when parsed into list of S-expressions.

Interpreter has no knowledge of special characters used in input stream, like `'`, `` `  `` or `,`
(single quote, backquote and comma). This characters (called read macros) are handled by parser and expanded
into correct lisp forms before interpretation. For example `'expr` is expanded into (quote expr).
Users of interpreter can register their handlers to process more read macros.

As all Semblance forms are JVM Serializable objects, so parsed program can be saved as standard Java Object stream.
This allows to bypass translation phase for often-used programs and speed up execution.

Semblance is *case sensitive*

Symbols
-------

Symbols in the program can be bound to variables/functions/macros or can be unbound. Bound symbols are evaluated to
the value they are bound to, unbound symbols will cause run-time exception. The only exception to this rule are
keyword symbols. Keywords are just symbols which start with a `:` and always evaluate to themselves.

Consider following two short programs:

    (set v (+ 1 2))
    (println v)

and

    (println v)

and

    (println :v)

In the first program, symbol `v` is bound to result of computation of `1 + 2`. Statement `(println v)` will
have `v` evaluated prior to calling `println` function and output will be **3**

Second program does not have symbol `v` bound to any value, so exception will be thrown.

Last program will evaluate `:v` to itself and print **:v**

Semblance does not support `|...|` syntax for symbols.  


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


Data types
----------

  **NUMBERS**
  
  Semblance has two number types - `integer` and `double`. 
  
  Internally integers can be represented as signed 64-bit numbers (long) *or* arbitrary size numbers (BigInteger).
   
  If result of operation cannot fit in the 64 bits, then it will be widened to BigInteger automatically. If the
  result *can* fit into 64 bits (even if operands were BigIntegers) it will be transformed to long. This means that
  as long as your numbers fit into signed 64-bit number, you get better performance, however, if they don't fit
  you don't need to worry.
  
  Promoting to double is also done automatically if one of the operands is double.  
  
  **STRINGS**

  Strings are everything inside double quotes. Some special characters can be escaped with backslash `\`, like
  `t`,`n`,`r`,`f`,`b`. Escapes for any other characters will be ignored, so `\y` will be interpreted just as `y`.
  Double quote character inside the string should be escaped.
  
  Examples of the strings:
  
     "String"
     "S\tring" -> S<tab code>ring
     "\"S\tri\ng\"" -> "S<tab code>tri<cr code>g"
  
  Multi-line strings are supported with triple double quotes. No escape processing is done for multi-line strings.
   
      """This is a
      multiline
      string"""

  **BOOLEAN**
  
  There is no boolean type per se in Semblance. There are two predefined symbols `T` and `F` which represent true 
  and false values.
  
  `Semblance false` is any value or expression which evaluates either to `F` or to `NIL`.
  `Semblance truth` is any value which is not `Semblance false`.

  **LIST**
  
    (value value ...)
    
  List is a function itself. Expression `(list-var 3)` will return value of the fourth list element (lists are 
  zero-based)
  
    (let ((x '(1 2 3))) 
         (x 1))
    ('(1 2 3) 1)
    
  List can be defined using function `list`. `list` will evaluate its parameters, while `quote` operation will not
    
    '( (+ 2 3))      => ((+ 2 3))
    (list (+ 2 3))   => ( 5 )
    
  Both expressions above will return **2**
  
  `NIL` expression always evaluates to empty list.
    
  **VECTOR**
    
    [value value ...]
    
  Vector is a function as well, so examples of index-based access of lists above can be applied to vectors as well.
  
    (let ((x [1 2 3])) 
         (x 1))
    ([1 2 3] 1)
    
  If index is negative it will be treated as index from the end of the list. 
  
    ('(1 2 3 4) -1) --> 4
    
  Vectors and lists support additional operations. For example if supplied with two indices it will return a slice of the
  list.
  
    ('(1 2 3 4) 1 2) --> (2 3)
    
  Some of operations are denoted by keywords, for example `head` (or `car`) and `tail` (or `cdr`) are implemented as
  collection operations `:head` and `:tail` (of course `car` and `cdr` are still supported)
  
    ('(1 2 3) :head) --> 1
    ('(1 2 3) :tail) --> (2 3)
    
  Other supported operations are:
    
   - `:length` - take number of elements in a collection
   - `:last` - take the last element of collection 
   - `:reverse` - reverse the collection
   - `:take n` - take first `n` elements of collection
   - `:drop n` - take all but first `n` elements of collection
   - `:sort` - sort collection. `:sort` may accept additional parameter `:desc` to sort in descending order. Attempt to
     sort a collection which holds elements of different types will cause runtime error.
   - `:map` - apply a function to each element of the collection. Function that accepts one parameter
     should be supplied as additional parameter to `:map`
   - `:filter` - apply a predicate function to each element of the collection. If function returns **T** then
     that element is included in the resulting collection. `:filter` requires one parameter that is 
     unary function that returns either **T** or **F**.
   - `:append` - adds parameters to a collection. If there is only one parameter and that parameter is a collection,
     then this parameter will be unwrapped and its elements will be appended to a collection
   - `:prepend` - same as append, but elements are inserted at the beginning of the collection. Both `:append` and 
     `:prepend` create new collection and do not modify existing
    
  Some of keyword operations can accept parameters.
  
    ('(1 2 3) :length) --> 3
    ('(1 2 3) :drop 2) --> (3)
    ('("bz" "ac" "ba") :sort) --> ("ac", "ba", "bz")
    ('("bz" "ac" "ba") :sort :desc) --> ("bz", "ba", "ac")
    
    (let ((double (fn (x) (* 2 x))))
         ('(1 2 3) :map double) )  --> (2 4 6)
         
    (let ((is-odd (fn (x) (!= 0 (% x 2)) )) )
         ('(1 2 3 4) :filter is-odd)) --> (1 3)
         
    ('(1 2) :append 3 4) --> (1 2 3 4)
    ([1 2] :append '(3 4)) --> [1 2 3 4]
    ([3 4] :prepend '(1 2)) --> [1 2 3 4]
    ('(3 4) :prepend 1 2) --> (1 2 3 4)
    
  Collection operations like `:reverse`, `:sort`, `:map` or `:filter` do not modify collection which they work on, 
  they create a new one.
    
  Though lists and vectors support the same operations their performance can be very different. Vectors are fast when
  accessing items by index, lists are fast when evaluating tail or appending/prepending values.
  
  Vectors are implemented as arrays while lists are linked lists.
    
  **MAP**
  
    {key value key value ... ...}
    
  Keys can be anything, but customary they are keywords. Map is a function, just like lists or vectors.
  Map can accept either one or two parameters. If one parameter is supplied then it is treated as map key and 
  corresponding value (or NIL) is returned. If two parameters are supplied, then first is treated as key and the 
  second one as a value and that value is stored in the map associated with a key.
  
    ({:a 1 :b 2 :c 3} :b)
    
  will return **2**
  
    ({:a 1 :b 2 :c 3} :b 5)
    
  will associate key **:b** with value **5** and will return value **5** as well.
  
      
  There are more operations available for collections.
  
  **CONTAINS**
  
    (contains collection element)
    
  Checks whether `collection` contains an `element`. If `collection` is a map, then check if key equal to `element` is
  present in the map. For list and vector linear search for element is performed.
  Result is either **T** or **F**
  
    (contains {:a 1 :b 2} :a) --> T
    (contains {:a 1 :b 2} :c) --> F
    (contains '(1 2 3) 2) --> T
    (contains [1 2 3] 4) --> F
    
  **MAP/KEYS** and **MAP/VALUES**
    
    (map/keys map-form)
    (map/values map-form)
    
  `keys` and `values` are map-specific operations. Former retrieves keys from a map as list, latter does the same
  for values. Order of elements in those lists is not defined.
  
    (map/keys {:a 1 :b 2 :c 3}) --> (:b :a :c) or (:a :c :b) or any other combination
    (map/values {:a 1 :b 2 :c 3}) --> (1 2 3) or (2 1 3) or any other combination

  More list/vector-specific operation are available in `list` namespace
  
  **LIST/FIND**
  
    (list/find collection element)

  Returns zero-based index of `element` in the `collection`. If there is no `element` in the `collection` then **-1** 
  is returned. `find` works on both lists and vectors. Bear in mind that access by index is O(n) for lists 
  and O(1) for vectors.
  
    (list/find '(1 2 3) 3) --> 2
    (list/find [1 2 3] 4) --> -1 
    
  **LIST/ZIP**
    
    (list/zip collection*)
    
  Returns list of lists where i-th list contains i-th element from each of the argument collections. Returned list has
  same length as the first collection. If any other collection is shorter than first one, `NIL` will be placed instead
  of its elements.
  
    (list/zip [1 2 3] ["a"] [:b :c]) -> ((1 "a" :b) (2 NIL :c) (3 NIL NIL))


Functions
---------

  Semblance probably includes more special forms than Common Lisp. That's done mostly for efficiency and sometimes
  to avoid complex parameter processing. Some of the special forms may later be rewritten as functions
  or macros in standard library.

  For the user of the language it should not matter whether operation is implemented as special form, function or a
  macro.
  
  **+**
  
    (+ parameter*)
  
  Addition, union or concatenation, depending on first parameter.
  
  Depending on type of first parameter following operations are performed:
    - Number - addition.
    - String - concatenation.
    - Map - union
    - Collection - union. resulting collection will be of the same type as first parameter. This means you can 
      compute union of list and vector.
        
          (+ '(1 2) [3 4])  -> (1 2 3 4)
          (+ [1 2] '(3 4))  -> [1 2 3 4]
          
    
  If any of the other parameters' type is different from the first parameter's type then `TypeMismatchException` will
  be thrown. 
  
    (+ 1 2 3) -> 6
    (+ "1" "2" "3") -> "123"
    (+ '(1 2) [3]) -> '(1 2 3)
    (+ [1 2] '(3)) -> [1 2 3]
    (+ {:a 1} {:b 2}) -> {:a 1 :b 2}
   
  **-**
   
    (- param*)
    
  Arithmetic subtraction or collection subtraction or negation. 
  
  If the first parameter is number, then subtracts second parameter from the first, then, if there are more 
  parameters, subtracts third parameter from the result, etc.
  If there is only one number parameter then negation of that parameter is returned.
  If the first parameter is collection then this operation will return collection of the same type as first parameter
  which will be a complement of all other parameters in first parameter. 
  
    (- 3) -> -3
    (- -3) -> 3
    (- 3 2 1) -> 0
    (- '(1 2 3 4) [1] '(2 4)) -> (3)
    (- [1 2 3] '(1 5)) -> (2 3)
    (- {:a 1 :b 2} {:b 1 :c 2}) -> {:a 1}
    
  **\***
  
    (* param*)
    
  Arithmetic multiplication on numbers or scalar or cartesian product on collections.
  If the first parameter is number, then arithmetic product of all parameters is returned.
  If the first parameter is collection then only one more parameter is expected, which can be either number or another
  collection. If the second parameter is number, then scalar product of list or vector and that number is evaluated,
  otherwise cartesian product of collections is evaluated.
  
    (* 1 2 3) -> 6
    (* '(1 2 3) 2) -> (2 4 6)
    (* [1 2] '("a" "b")) -> [(1 "a") (1 "b") (2 "a") (2 "b")]

  **Comparison functions**
   
    (= param*)
    (!= param*)
    (> param*)
    (< param*)
    (>= param*)
    (<= param*)

  Evaluates parameters and returns `T` if all parameters satisfy the condition, i.e.
        
    (= "a" "a" "a") -> T
    (= "a" "a" "b") -> F
    (> 5 4 3 2 1) -> T
    (> 5 4 3 1 2) -> F
    
    
  **AND** **OR**
    
    (and params*)
    (or params*)
    
  Logical operations `AND` and `OR`. Will evaluate its parameters and perform logical operation on `Semblance boolean`
  values, returning either `T` or `F`. 
   
  `AND` and `OR` do short-circuit and will not evaluate more parameters than needed.   
  
  **NOT**
  
    (**NOT** param)
    
  Logical `NOT`. Only accepts one parameter as opposed to `AND` and `OR`.
    
  **PRINTLN**

    (println value1 value2 value3 ...)

  prints all values separated by space and returns NIL

  **LET**

    (let variables &rest body)

  where `variables` is list of (name value) definitions and `body` is sequence of S-expressions.
  Example:

    (let ((x 40)
          (y 2))
         (+ x y))

  Let is similar to Common Lisp `let*`, all bindings are done one by one, not in parallel. Let evaluates to last
  value in `body`

  **IF**

    (if condition then-expr &optional else-expr)

  Evaluates condition, if it evaluates to `Semblance Truth`, then executes then-expr, if condition is 'Semblance False' 
  and else-expr is present it will be executed

     (if (= a b)
         (progn ()()...)
         (println "False"))
         
     (if 1
         (prinln "True")
         (prinln "False"))  => prints "True"
         
  **COND**
  
    (cond
        (condition-1 expr1-1 expr1-2 expr1-3)
        (condition-2 expr2-1 expr2-2)
        (condition-3 expr3-1)
        ...
    )
      
  Evaluates `condition-1`, if it evaluates to `semblance truth` then all expressions will be executed in implicit 
  `progn` block. If `condition-1` evaluates to `semblance false` then next condition will be evaluated (i.e. `condition-2`)
  
  If none of the conditions evalutes to `truth`, then `cond` will return `NIL`, otherwise result of the last expression
  of the matching block will be returned.
  
  
  **SELECT**
  
    (select
        (block-1)
        (block-2)
        ...
        (block-N)
    )
    
  Evaluates blocks one by one, returning the result of the first one which is not `NIL`. As soon as block returning 
  non-`NIL` is found any other blocks are not evaluated.

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
  
    (progn
      (println "1")
      (println "2")
      (+ 1 1))  -> 2 (and prints "1" and "2")

  **FN**

    (fn (parameters) (s-expression)*)

  Defines a nameless function.
  
  **FN\***
  
    (fn* (s-expression)*)
  
  Defines a nameless function with a single implicit parameter named `it`.
  
    ((fn* (* it it)) 3)
    
  Will evalute square of **3** and return **9**

  **NAMESPACE**

     (namespace name (s-expression)*)

  Declares a new namespace with a name `name` in current context. Executes body and after execution binds
  all declared symbols in the new namespace.
  
  **USE**
  
     (use namespace (s-expression)*)
     
  Creates new block and imports all binding from `namespace` into this block, so they are available without namespace
  reference.
  
     (use list
       (find [1 2 3] 3)
     ) 
     
  **FUNCALL**
  
  Calls a function.

     (funcall function (parameters))

  For example:

     (defun mult2 (x) (* 2 x))
     (funcall mult2 (4))

     or

     (funcall
        (fn (x) (* 2 x))
        (4))
    
  **DEFUN**
  
    (defun name (params) (s-expression)*)
    
  Creates a function with parameters `params` and body defined by a list of expressions and binds it to name `name` in current context.
  Example:
    
    (defun add (x y) (+ x y))
    (add 1 2)
    
  will result in **3**
  
  **RECUR**
  
    (recur values*)
    
  Initiates recursive call of the enclosing function. Program behavior is undefined if `recur` is called outside of 
  function.
  Consider following function
  
    (defun rec (x) 
          (if (= 0 x) "Tada" 
                      (rec (- x 1))))
  
  It will call itself, decreasing its parameter by one and once reached *0* it will terminate returning string "Tada".
  This will work well if function `rec` is called with parameter less than [about] 500. If called as `rec(1000)` the
  program will crash with StackOverflow exception.
  `recur` is similar to its namesake in Clojure, it evaluates its parameters, then rebinds them and jumps back to the 
  recursion point. No check is performed to verify that `recur` is in tail position, however if there are any operators
  after the `recur` they will never receive control. The recur expression must match the arity of the recursion point,
  i.e. of the function inside which it is called. 
  
  See next code block for an example of `recur` usage.
   
    (defun rec (x) 
          (if (= 0 x) "Tada" 
                      (rec (- x 1))))
    (defun rec2 (x) 
          (if (= 0 x) "Tada" 
                      (recur (- x 1))))

    (rec 1000) --> Will fail
    (rec2 5000) --> Will complete successfully
  

  **SET**

    (set (name value) [(name value) ...])
  Binds evaluated `value` to symbol `name` in the current context. May evaluate and bind multiple variables.
  Evaluation is done sequentially, so later binds have access to already bound variables. 
  Values are optional, if value is omitted `NIL` will be bound to the name. 
  Returns latest evaluated value.
  Examples:
  
     (set (pi 3.14) (tau 6.28) (e 2.71)) -> 2.71
     (set (empty) (also-empty)) -> NIL
     (set empty also-empty)   -> NIL
     
  Last variant is a shortened version of the second one. 
  
  
  **SET1**
  
    (set1 name expression)

  Binds evaluated `expression` to a `name` in current context. Essentially works as `set`, but only for one variable.
  `expression` cannot be omitted.
  
  
  **SET\***
  
    (set* (name value)*)
    
  Both `set` and `set1` work in current context. If you need to change value of a global binding you need to use `set*`.
  `set*` will first try to find existing binding with a name `name` and rebind new value to it. If there is no
  existing binding with such name in all outer scopes, then new local binding will be created.
  
    (let (x 1)
         (progn ; new block
            (set* (x 2))  
         )
         (println x) --> prints 2
    )     

    (let (x 1)
         (progn ; new block
            (set (x 2))  ; creates new binding in progn block context
         )
         (println x) --> prints 1
    )     
  
  **NAMESPACE**

    (namespace name (s-expression)*)

  Declares namespace named `name`. All variable, function and macro binding in the s-expressions will be bound
  inside this namespace. Following example shows function `pi` and variable `e` bound in `math` namespace:

    (namespace math
       (set1 e 2.718)
       (defun pi () 3.14))

  Such bindings can be accessed using full name

    (* 2 math/pi)
    (println math/e)

  Inside the namespace those names can be accessed without the qualifier.

    (namespace math
       (set1 pi 3.14))
       (set1 tau (* 2 pi)))

  **BLOCK**

    (block name forms*)

  Defines a named block. Last evaluated value is returned from block unless `return` is called inside.

  **RETURN**

    (return [value])

  Returns from block. Block is defined by `block`, `progn`, `let`, `fn` or `namespace`. Optional return value can
  be supplied. If no return value is supplied, then `NIL` will be returned. If `return` is called outside of the block,
  then program itself will terminate.

    (let ((x 1)
          (y 2))
         (return y)
         x)

  Result of the `let` expression above will be 2, not 1.

  **RETURN-FROM**

    (return-from block-name [value])

  Returns from a named block. Named block is defined by `block`, `defun` or `namespace`. If value is not supplied `NIL`
  will be returned. If there is no block named `block-name` in the execution stack then program will terminate and
  value supplied to `return-from` will be returned.

    (block b1
        (block b2
            (block b3
                (return-from b1 5))
         2 )
     3)

  will return **5**.

    (block b1
        (block b2
              (block b3
                    (return-from b2 5))
         2 )
     3)

  will return **3**, as atom **3** will be last thing evaluated after execution of block `b2` is terminated by `return-from b2`.
  
  **LOOP**
  
    (loop condition-expression (s-expression)*)
    
  Loops through list of `s-expressions` while `condition-expression` evaluates to `T`.
  To break out of the loop use `break` special form. `return` form will not work as loop creates implicit
  block which will eat return value and return to `condition-expression` evaluation.
      
      (let ((x 5) (y 0))
           (loop (> x 0)
                 (set x (- x 1)) 
                 (set y (+ y 1)) 
                 (if (= y 3)
                     (break y)) 
      )) 
      
  The program above will return **3**
  
  
  **DO-LIST**
    
    (do-list list-form element &rest body)
    
  Loops over `list-form` iteratively binding its elements to `element` symbol and executing `body`.
  
    (set res) 
    (do-list [1 2 3] 
             x 
             (set* (res 
                      (res :prepend x))
             )
    ) 
    
  `res` will contain **(3 2 1)** after this program is executed 
  
  **DO-TIMES**
    
    (do-list element upper-limit &rest body)
    
  Assigns zero to `element` and executes a body. Increases `element` by one and repeats execution of a body
  while `element` is less than `upper-limit`
  
    (set res) 
    (do-times x 
              3 
              (set* (res 
                        (res :append x))
              )
    ) 
    
  `res` will contain **(0 1 2)** after this program is executed
     
Macros
------

  Currently there is no macro expansion phase in the program execution time, however there are plans to introduce
  such phase. In the time being macros are expanded as they are executed.

  **DEFMACRO**

    (defmacro name (parameters) (s-expression)*)

  Defines a macro named `name` with a list of formal `parameters` and body of s-expressions. Just like
  other lisp out there you can use backquote to escape macro body.
  You need to use comma to un-escape parameters and you can still use `@` to unwrap lists.

    (defmacro defun
         (name params &rest body)
         `(set1 ,name (fn ,params ,@body)))

  Example defines a `defun` macro which takes three parameters `name`, `params` and `body` and binds to symbol
  in `name` a function with parameters `params` and `body` of s-expressions.


Standard Library
----------------

### Java interop

  See separate document java-interop.md

### Concurrent

  Semblance has some basic features allowing to execute code concurrently. How much concurrency is there depends 
  on underlying JVM implementation.
 
  Concurrent constructs are defined in `async` namespace and are available only if interpreter was started with
  `Concurrent` context modifier.

  Concurrent operations usually return `future` which is an opaque wrapper containing value somewhere inside.
  Value can be extracted using `async/await`.
  
  Concurrent library is not complete and in development.
  
  **ASYNC/RUN**
  
    (async/run (s-expression)*)
    
  Runs s-expressions concurrently, returns a future.
  
    (async/run (+ 2 2)) ---> future, which will eventually return 4
  
  **ASYNC/GO**
  
    (async/go function (parameters))
    
  Concurrently performs a function call, providing function with supplied parameters.
   
    (async/go + (2 2))                  --> future, which will eventually return 4
    (async/go (fn (x y) (+ x y)) (2 2)) --> future, which will eventuall return 4
   
  **ASYNC/CHECK**
  
    (async/check future-value)
    
  Checks if the future-value has been computed. Returns either `T` or `F`
  
    (set1 x (async/go + (2 2)))  --> x will be bound to a future
    (async/check x)              --> check will return T if future is computed and its value can be retrieved 
    
  **ASYNC/AWAIT**
   
    (async/await future-value)
    
  Waits till the future-value has been computed. Returns future value or NIL, if future failed
  
    (set1 x (async/go + (2 2)))  --> x will be bound to a future
    (async/await x)              --> will wait till the future is computed and then return 4
   
  **ASYNC/DELAY**
  
    (async/delay ms)
    
   Puts current thread to sleep for given number of milliseconds
   
    (async/delay 500) --> delay for half a second