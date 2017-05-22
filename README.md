# Semblance #
## Version 2 ##

### What is it? 

Semblance is an small Lisp machine written in Java and intended for embedding in Java.


### Features 

Semblance is not adhering to any particular Lisp standard, it is mostly following Common Lisp with some features from Clojure, like vectors and maps.


### Project structure

 - *semblance-code* - Interpreter and standard library
 - *semblance-concurrent* - Concurrent library (futures, async/await)
 - *semblance-java* - Java integration. Embedding
 - *semblance-cli* - REPL
 - *semblance-edit* - UI Editor

### Requirements

Semblance 2.0.0 requires Java 8