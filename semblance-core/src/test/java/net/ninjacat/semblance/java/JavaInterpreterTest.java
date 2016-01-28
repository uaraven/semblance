package net.ninjacat.semblance.java;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.OpaqueValue;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import org.hamcrest.core.IsCollectionContaining;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.symbol;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;

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

    @Test(expected = SemblanceRuntimeException.class)
    public void testShouldFailToCreateUnknownJavaObject() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        interpreter.run("(java/new net.ninjacat.semblance.java.Pojo2)");
    }

    private Interpreter createJavaInterpreter() {
        return new Interpreter(new JavaBridge());
    }
}
