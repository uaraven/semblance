Reader todo
-----------
 
 - document numbers
 - document strings
 - document case-sensivity and the absense of |<text>|
 - wrap program into additional named block with the name of file, i.e.
      (--file-block "file-name" ()...())
   and implement --file-block macro to bind "file-name" to --source-file-name variable
 
Evaluator todo
--------------

 - symbols should evaluate to variables (if no variable is defined, then throw exception?)
 - :keywords evaluate to their string representation (same as autobinding to string with the same value)
 