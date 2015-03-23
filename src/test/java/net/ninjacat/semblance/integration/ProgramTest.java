package net.ninjacat.semblance.integration;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.Macro;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Created on 03/03/15.
 */
public class ProgramTest {

    @Test
    public void testShouldDoSimpleArithmetic() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("(+ 2 (- 4 2))");

        assertThat(value, is(number(4)));
    }

    @Test
    public void testFunctionShouldWork() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("(var (f (fn (x y) (+ x y)))) (f 2 1)");

        assertThat(value, is(number(3)));
    }

    @Test
    public void testShouldDefineMacro() throws Exception {
        final Interpreter interpreter = new Interpreter();
        final LispValue value = interpreter.run("(defmacro mcr (x y) `(,x (+ 1 ,y)))");

        assertThat(value, instanceOf(Macro.class));

        final Macro macro = (Macro) value;
        assertThat(macro.name(), is(symbol("mcr")));
    }

    @Test
    public void testShouldDefineAndExecuteMacro() throws Exception {
        final Interpreter interpreter = new Interpreter();
        final LispValue value = interpreter.run("(defmacro mcr (x y) `(,x (+ 1 ,y)))\n(mcr quote 2)");

        assertThat(isList(value), is(true));
        assertThat(asSList(value), is(list(symbol("+"), number(1), number(2))));
    }
}
