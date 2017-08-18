# Semblance standard library

## IO

## Collections

## String functions

**INDEX-OF**
    
     (index-of <string> <substring> [<starting-index>])

 Returns the index within this string of the first occurrence of the specified substring, starting at the specified index.
 If no such index exists, then -1 is returned.

     (index-of "abcdefg" "cde") --> 2
     (index-of "abcdefg" "xyz") --> -1
     (index-of "abcabcd" "abc" 1) --> 3

**SUBSTRING**

      (substring <string> <start-index> [<end-index>])
      
  Returns a string that is a substring of this string. The substring begins at the specified `start-index` and extends to the character at index `end-index - 1`. 
  Thus the length of the substring is `end-index` - `start-index`.
  If `end-index` is omitted, returns substring from `start-index` till the end of original string
  
         (substring "hamburger" 4 8) --> "urge"
         (substring "smiles" 1  5) --> "mile"
         (substring "steamship" 5) --> "ship"
         
**REPLACE**

**REPLACE-ALL**

**SPLIT**

**JOIN**

**TO-LOWER**

**TO-UPPER**

**EQ-IGNORE-CASE**

## Regular expressions

Regular expression flags:

 - unix-lines
 - case-insensitive
 - comments
 - multiline
 - literal
 - dotall
 - unicode-case
 - canon-eq
 - unicode-character-class

**RE/REGEX**

**RE/MATCH**
 
**MATCH**

**FIND**

## Math functions

### Constants

**E**

**PI**

### Functions

**SIN**

**COS**

**TAN**

**ASIN**

**ACOS**

**ATAN**

**ATAN2**

**EXP**

**LOG**

**SQRT**

