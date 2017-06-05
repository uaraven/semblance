package net.ninjacat.semblance.java;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.OpaqueValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import org.hamcrest.Matchers;
import org.hamcrest.core.Is;
import org.hamcrest.core.IsCollectionContaining;
import org.junit.Test;

import static net.ninjacat.semblance.java.JavaInterop.toLisp;
import static net.ninjacat.semblance.java.SEnum.Value1;
import static net.ninjacat.semblance.java.SEnum.Value2;
import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;

@SuppressWarnings("InstanceMethodNamingConvention")
public class JavaInterpreterTest {

    @Test
    public void testShouldHaveJavaNamespace() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        assertThat(interpreter.getRootContext().findNamespace(symbol("java")), notNullValue());
    }

    @Test
    public void testShouldCreateJavaObjectWithDefaultConstructor() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final LispValue value = interpreter.run("(java/new net.ninjacat.semblance.java.Pojo)");

        assertThat(value, instanceOf(OpaqueValue.class));
        assertThat(((OpaqueValue) value).getValue(), instanceOf(Pojo.class));
    }

    @Test
    public void testShouldCreateJavaObjectWithParametrizedConstructor() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final LispValue value = interpreter.run("(java/new net.ninjacat.semblance.java.Pojo 10 \"Woho\" 4.2)");

        assertThat(value, instanceOf(OpaqueValue.class));
        assertThat(((OpaqueValue) value).getValue(), instanceOf(Pojo.class));
        final Pojo pojo = (Pojo) ((OpaqueValue) value).getValue();

        assertThat(pojo.getIntValue(), is(10));
        assertThat(pojo.getStringValue(), is("Woho"));
        assertThat(pojo.getDoubleValue(), is(4.2));
    }

    @Test
    public void testShouldCreateJavaObjectWithObjectConstructor() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        interpreter.getRootContext().bind(symbol("pojo"), new OpaqueValue<>(new Pojo(1, "2", 3.3)));

        final LispValue value = interpreter.run("(java/new net.ninjacat.semblance.java.Pojo pojo)");

        assertThat(value, instanceOf(OpaqueValue.class));
        assertThat(((OpaqueValue) value).getValue(), instanceOf(Pojo.class));
        final Pojo pojo = (Pojo) ((OpaqueValue) value).getValue();

        assertThat(pojo.getIntValue(), is(1));
        assertThat(pojo.getStringValue(), is("2"));
        assertThat(pojo.getDoubleValue(), is(3.3));
    }

    @Test
    public void testShouldCreateJavaObjectWithListInParameter() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final LispValue value = interpreter.run("(java/new net.ninjacat.semblance.java.CollectionPojo '(1 2 3))");

        assertThat(value, instanceOf(OpaqueValue.class));
        assertThat(((OpaqueValue) value).getValue(), instanceOf(CollectionPojo.class));
        final CollectionPojo cpojo = (CollectionPojo) ((OpaqueValue) value).getValue();
        assertThat(cpojo.getData(), IsCollectionContaining.hasItems(1, 2, 3));
    }

    @Test
    public void testShouldCreateJavaObjectWithArrayInParameter() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final LispValue value = interpreter.run("(java/new net.ninjacat.semblance.java.ArrayPojo '(1 2 3))");

        assertThat(value, instanceOf(OpaqueValue.class));
        assertThat(((OpaqueValue) value).getValue(), instanceOf(ArrayPojo.class));
        final ArrayPojo cpojo = (ArrayPojo) ((OpaqueValue) value).getValue();
        assertThat(cpojo.getData(), is(new int[]{1, 2, 3}));
    }

    @Test
    public void testGetIntField() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        interpreter.getRootContext().bind(symbol("x"), new JavaWrapperValue(new Pojo(1, "2", 3.3)));

        final LispValue value = interpreter.run("(x intValue)");

        assertThat(value, is(number(1)));
    }

    @Test
    public void testGetStringField() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        interpreter.getRootContext().bind(symbol("x"), new JavaWrapperValue(new Pojo(1, "2", 3.3)));

        final LispValue value = interpreter.run("(x stringValue)");

        assertThat(value, is(string("2")));
    }

    @Test
    public void testGetDoubleField() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        interpreter.getRootContext().bind(symbol("x"), new JavaWrapperValue(new Pojo(1, "2", 3.3)));

        final LispValue value = interpreter.run("(x doubleValue)");

        assertThat(value, is(number(3.3)));
    }

    @Test
    public void testGetBooleanField() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final Pojo pojo = new Pojo();
        interpreter.getRootContext().bind(symbol("x"), new JavaWrapperValue(pojo));

        final LispValue value = interpreter.run("(x boolValue)");

        assertThat(value, Is.<LispValue>is(Constants.TRUE));
    }

    @Test
    public void testSetBooleanField() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final Pojo pojo = new Pojo();
        interpreter.getRootContext().bind(symbol("x"), new JavaWrapperValue(pojo));

        interpreter.run("(x boolValue F)");

        assertThat(pojo.isBoolValue(), is(false));
    }

    @Test
    public void testSetIntField() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final Pojo pojo = new Pojo(10, "20", 30.3);
        interpreter.getRootContext().bind(symbol("x"), new JavaWrapperValue(pojo));

        interpreter.run("(x intValue 150)");

        assertThat(pojo.getIntValue(), is(150));
    }

    @Test
    public void testSetIntFieldViaSet() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final Pojo pojo = new Pojo(10, "20", 30.3);
        interpreter.getRootContext().bind(symbol("x"), new JavaWrapperValue(pojo));

        interpreter.run("(java/set x intValue 150)");

        assertThat(pojo.getIntValue(), is(150));
    }

    @Test
    public void testSetStringField() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final Pojo pojo = new Pojo(10, "20", 30.3);
        interpreter.getRootContext().bind(symbol("x"), new JavaWrapperValue(pojo));

        interpreter.run("(x stringValue \"newstr\")");

        assertThat(pojo.getStringValue(), is("newstr"));
    }

    @Test
    public void testSetDoubleField() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final Pojo pojo = new Pojo(10, "20", 30.3);
        interpreter.getRootContext().bind(symbol("x"), new JavaWrapperValue(pojo));

        interpreter.run("(x doubleValue 42.01)");

        assertThat(pojo.getDoubleValue(), is(42.01));
    }

    @Test
    public void testGetIntArrayField() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final LispValue value = interpreter.run("(set1 x (java/new net.ninjacat.semblance.java.ArrayPojo '(1 2 3))) (x data)");

        assertThat(value, instanceOf(SList.class));
        assertThat(value, Is.<LispValue>is(smartList(1L, 2L, 3L)));
    }

    @Test
    public void testGetStrArrayField() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final ArrayPojo pojo = new ArrayPojo();
        pojo.strData = new String[]{"one", "two"};

        interpreter.getRootContext().bind(symbol("pojo"), new JavaWrapperValue(pojo));

        final LispValue value = interpreter.run("(pojo strData)");

        assertThat(value, instanceOf(SList.class));
        assertThat(value, Is.<LispValue>is(smartList("one", "two")));
    }

    @Test
    public void testGetStrFieldViaGet() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        interpreter.getRootContext().bind(symbol("x"), new JavaWrapperValue(new Pojo("striiing")));
        final LispValue value = interpreter.run("(java/get x stringValue)");

        assertThat(value, is(string("striiing")));
    }

    @Test
    public void testSetStrArrayField() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final ArrayPojo pojo = new ArrayPojo();
        pojo.strData = new String[]{"one", "two"};

        interpreter.getRootContext().bind(symbol("pojo"), new JavaWrapperValue(pojo));

        interpreter.run("(pojo strData '(\"1\" \"2\"))");

        assertThat(pojo.strData, Matchers.arrayContaining("1", "2"));
    }

    @Test
    public void testMethodCall() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final Pojo pojo = new Pojo();

        interpreter.getRootContext().bind(symbol("pojo"), new JavaWrapperValue(pojo));

        final LispValue value = interpreter.run("(pojo power 5 2)");

        assertThat(value, Is.<LispValue>is(doubleN(25.0)));
    }

    @Test
    public void testMethodCallWithNumberCast() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final Pojo pojo = new Pojo();

        interpreter.getRootContext().bind(symbol("pojo"), new JavaWrapperValue(pojo));

        final LispValue value = interpreter.run("(pojo power 5.0 2.0)");

        assertThat(value, Is.<LispValue>is(doubleN(25.0)));
    }

    @Test
    public void testMethodCallWithArraysAndStrings() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final Pojo pojo = new Pojo();

        interpreter.getRootContext().bind(symbol("pojo"), new JavaWrapperValue(pojo));
        interpreter.getRootContext().bind(symbol("v"), Constants.TRUE);

        final LispValue value = interpreter.run("(pojo toStr '(\"No\" \"Yes\") v)");

        assertThat(value, is(string("Yes: true")));
    }

    @Test
    public void testStaticMethodCall() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final LispValue value = interpreter.run("(java/scall net.ninjacat.semblance.java.Pojo.name)");

        assertThat(value, is(string("Pojo")));
    }

    @Test
    public void testStaticMethodCallWithParams() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        final LispValue value = interpreter.run("(java/scall net.ninjacat.semblance.java.Pojo.toStr (list 1 2 3 4))");

        assertThat(value, Is.<LispValue>is(smartList("1", "2", "3", "4")));
    }

    @Test
    public void testStaticFieldGet() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        //noinspection AssignmentToStaticFieldFromInstanceMethod
        Pojo.sLong = 42L;
        final LispValue value = interpreter.run("(java/get net.ninjacat.semblance.java.Pojo.sLong)");

        assertThat(value, Is.<LispValue>is(longN(42)));
    }

    @Test
    public void testEnum() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();
        interpreter.getRootContext().bind(symbol("se"), toLisp(new EnumPojo()));

        final LispValue value = interpreter.run("(se get)");

        assertThat(value, Is.is(JavaInterop.toLisp(Value1)));
    }

    @Test
    public void testEnum2() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();
        interpreter.getRootContext().bind(symbol("se"), toLisp(new EnumPojo()));

        final LispValue value = interpreter.run("(se convert 'Value1)");

        assertThat(value, Is.is(JavaInterop.toLisp(Value2)));
    }

    @Test
    public void testStaticFieldSet() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        interpreter.run("(java/set net.ninjacat.semblance.java.Pojo.sStr \"Static\")");

        assertThat(Pojo.sStr, is("Static"));
    }

    @Test(expected = SemblanceRuntimeException.class)
    public void testShouldFailToCreateUnknownJavaObject() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        interpreter.run("(java/new net.ninjacat.semblance.java.Pojo2)");
    }

    private static Interpreter createJavaInterpreter() {
        return new Interpreter(new JavaBridge());
    }
}
