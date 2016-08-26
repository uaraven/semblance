# Semblance Java Interoperability guide

## Embedding Semblance interpreter

### Libraries

Core of Semblance interpreter is available as maven artifact 

    <dependency>
        <groupId>net.ninjacat</groupId>
        <artifactId>semblance-core</artifactId>
        <version>2.0.0</version>
    </dependency>

Additional artifacts provide java interoperability and concurrency libraries to use with core interpreter
 
    <dependency>
        <groupId>net.ninjacat</groupId>
        <artifactId>semblance-java</artifactId>
        <version>2.0.0</version>
    </dependency>
    <dependency>
        <groupId>net.ninjacat</groupId>
        <artifactId>semblance-concurrent</artifactId>
        <version>2.0.0</version>
    </dependency>

To use Semblance in your project include required libraries using method appropriate for your build system.

### Using interpreter

Semblance interpreter interface is provided with net.ninjacat.semblance.Interpreter class. It allows to run
Semblance programs from source code from `String` or `InputStream`, compile programs and run compiled programs.

Semblance extensions are implemented in the form of `ContextModifier`s. `ContextModifier` is an interface that
allows to modify root context of Semblance interpreter by registering namespaces, binding symbols and defining
functions and macros.
 
Java Interop is in `java` namespace and is provided by `net.ninjacat.semblance.java.JavaBridge` context modifier 
and `concurrency` namespace is provided by `net.ninjacat.semblance.concurrent.Concurrent` context modifier.

To instantiate an interpreter with both extensions use

    new Interpreter(new JavaBridge(), new Concurrency());
    
Every value in use by Semblance, including functions, macros, atoms, Java objects, etc. is an implementation of `ListValue`
interface. Class `net.ninjacat.semblance.utils.Values` provides a lot of methods to create `LispValue`s from Java values,
check `LispValue`s types and convert them back to Java values. See JavaDoc for details.


## Accessing Java objects from Semblance code

Access to Java is implemented in `java` namespace which is available 
in interpreter created with `JavaBridge` context modifier.

Semblance programs can create new instances of Java classes, read and modify fields
and call methods. It is also possible to call static methods.

### Type mapping

Semblance type system is much simpler than Java's so to interoperate with Java successfully some 
sort of smart type conversion is needed.

Semblance-Java interop uses following rules to perform type conversions:

    | Semblance Type |                             Java Type                             |
    |:--------------:|:-----------------------------------------------------------------:|
    |     Integer    | byte char short int long Byte Character Short Integer Long BigInt |
    |      Float     | float double Float Double                                         |
    |     String     | String                                                            |
    |     Symbol     | Boolean, but only for `T` and `F` constants                       |
    |      List      | Arrays, Collections, Iterables                                    |
    |     Vector     | Same as list                                                      |
    |      Map       | Map (not implemented)                                             |
    |     Opaque     | Object, if opaque value contains java object                      | 

Semblance will treat primitive types and boxed types the same way.

When method call is performed Semblance-Java will look for the first overloaded method which may accept parameters.
Let's take for example following java class

    class ExampleObject {
       void setValues(int a, int b, byte c);
       void setValues(long a, long b, int c);
    }
    
Both overloaded methods are different from Java perspective, but when used in following Semblance program

    (set1 obj (java/new net.example.ExampleObject))
    (obj setValues 120000000000 2 3)
   
setValues(int, int, byte) will be selected as it is the first matching method. Long value 120000000000 will be trimmed
to Java int type. 

If you need to call specific method, then possible solution will be to use full method signature:

    (obj setValues(IIB) 120000000000 2 3)
    
Same approach can be applied to constructors

    (java/new net.example.Example2(ILjava/lang/String;)

<font color='red'>CALL BY SIGNATURE IS NOT IMLEMENTED YET</font>

### Functions

 **JAVA/NEW**
 
 Creates and returns new instance of java class
   
     (java/new canonical-class-name (constructor parameters))
   
 Finds a constructor matching parameters and calls it. Returns java object wrapped
 into Semblance value.
 
     (set1 jstring (java/new java.lang.String "This string is created in Semblance")) --> creates new Java String
     (set1 bi (java/new java.util.BigInteger '(1 2 3))  --> Create new BigInteger calling BigInteger(byte[]) constructor
   

 **JAVA/GET**
 
 Retrieves value of the field. This works with both static and normal fields.
 
    (java/get object propertyName)
    (java/get canonical-class-name staticPropertyName)
    
 Retrieved value will be converted to corresponding Semblance type, iterables and arrays will be converted to `list`.
 
    (java/get arr length)
    (java/get java.util.BigInteger ONE)
    
 **JAVA/SET**
 
 Sets value of the field. This works for both static and normal fields.
 
     (java/set object propertyName value)
     (java/set canonical-class-name staticPropertyName value)
     
  Value will be converted to a proper Java type before assignment. If value's semblance type cannot be converted to 
  Java type (i.e. `string` to `int`) then `JavaTypeConversionException` will be thrown.
  
     (java/set page current 12)
     (java/set net.example.Static DAY "Monday")

 **METHOD CALL**
     
 Java wrapper value is a function in Semblance and object's methods can be called through the wrapper itself.
 
 Given
 
     (set1 s (java/new java.lang.String "This is string"))
 Call to
  
     (s length)
 Will perform java.lang.String.length() method call 
 
 Proper syntax is:
 
     (object-wrapper name param1, param2, ...)
     
All the parameters are optional. Value returned from the method is converted to a closest Semblance type. void methods will return `NIL`. 

If there is no method with name `name` with matching parameters found in the class, Semblance will attempt to 
_find a field_ with name `name`. If the field is found and there are no parameters in the call, then it will return field's value,
if *one* parameter was supplied and it is compatible with field's type, then field's value will be updated.
 
This allows shorter and more concise syntax for accessing fields. 
     
 **JAVA/SCALL**
 
    (java/scall canonical-class-name static-method-name param1, param2, ...)
    
 Performs a static method call. Works the same as with constructors and regular methods.
  
    (java/scall java.util.Calendar newInstance)
    (java/scall java.util.Calendar newInstance (java/new java.util.Locale "en-US"))