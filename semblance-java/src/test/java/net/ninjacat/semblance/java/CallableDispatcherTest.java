package net.ninjacat.semblance.java;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.string;
import static net.ninjacat.semblance.utils.Values.symbol;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;

public class CallableDispatcherTest {

    @Test
    public void shouldCallMethodAsSpecialForm() throws Exception {
        final Interpreter interpreter = new Interpreter(new JavaBridge());
        final Pojo1 pojo = new Pojo1();

        interpreter.getRootContext().bind(symbol("pojo"), pojo);
        final LispValue value = interpreter.run("(pojo concat \"abc\" \"def\")");

        assertThat("concat should be called", pojo.concatCalled, is(true));
        assertThat("concat2 should not be called", pojo.concat2Called, is(false));
        assertThat(value, is(string("abcdef")));
    }

    @Test
    public void shouldCallMethodAsSpecialFormUsingStringName() throws Exception {
        final Interpreter interpreter = new Interpreter(new JavaBridge());
        final Pojo1 pojo = new Pojo1();

        interpreter.getRootContext().bind(symbol("pojo"), pojo);
        final LispValue value = interpreter.run("(pojo \"concat\" \"abc\" \"def\")");

        assertThat("concat should be called", pojo.concatCalled, is(true));
        assertThat("concat2 should not be called", pojo.concat2Called, is(false));
        assertThat(value, is(string("abcdef")));
    }

    @Test
    public void shouldCallMethodAsFunction() throws Exception {
        final Interpreter interpreter = new Interpreter(new JavaBridge());
        final Pojo1 pojo = new Pojo1();

        interpreter.getRootContext().bind(symbol("pojo"), pojo);
        final LispValue value = interpreter.run("(pojo concat2 \"abc\" \"def\")");

        assertThat("concat should not be called", pojo.concatCalled, is(false));
        assertThat("concat2 should be called", pojo.concat2Called, is(true));
        assertThat(value, is(string("abcdef")));
    }

    @Test
    public void shouldCallMethodAsFunctionUsingStringName() throws Exception {
        final Interpreter interpreter = new Interpreter(new JavaBridge());
        final Pojo1 pojo = new Pojo1();

        interpreter.getRootContext().bind(symbol("pojo"), pojo);
        final LispValue value = interpreter.run("(pojo \"concat2\" \"abc\" \"def\")");

        assertThat("concat should not be called", pojo.concatCalled, is(false));
        assertThat("concat2 should be called", pojo.concat2Called, is(true));
        assertThat(value, is(string("abcdef")));
    }


    @Test(expected = JavaInteropException.class)
    public void shouldFailOnNonName() throws Exception {
        final Interpreter interpreter = new Interpreter(new JavaBridge());
        final Pojo1 pojo = new Pojo1();

        interpreter.getRootContext().bind(symbol("pojo"), pojo);
        interpreter.run("(pojo 2 \"abc\" \"def\")");
    }

    @Test(expected = SemblanceRuntimeException.class)
    public void shouldFailOnNonexistingMethod() throws Exception {
        final Interpreter interpreter = new Interpreter(new JavaBridge());
        final Pojo1 pojo = new Pojo1();

        interpreter.getRootContext().bind(symbol("pojo"), pojo);
        interpreter.run("(pojo invalidMethod \"abc\" \"def\")");
    }


}