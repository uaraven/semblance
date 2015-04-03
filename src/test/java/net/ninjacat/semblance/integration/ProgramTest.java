package net.ninjacat.semblance.integration;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.Macro;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Interpreter integration test. Here should go tests which verify correctness of interpreter, context and
 * built-in special forms. Tests that verify standard library should be placed in {@link StandardLibraryTest}
 */
public class ProgramTest {

    @Test
    public void testShouldDoSimpleArithmetic() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("(+ 2 (- 4 2))");

        assertThat(value, is(number(4)));
    }

    @Test
    public void testShouldExecuteLambda() throws Exception {
        final Interpreter interpreter = new Interpreter();
        final LispValue value = interpreter.run(
                "(funcall (fn (x y)" +
                        "             (+ x y))" +
                        "          (2 3))");

        assertThat(value, is(number(5)));
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


    @Test
    public void testFunctionShouldWork() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("(var (f (fn (x y) (+ x y)))) (f 2 1)");

        assertThat(value, is(number(3)));
    }

    @Test
    public void testSimpleReturnShouldWork() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("(let ((x 1) (y 2)) (return y) x)");

        assertThat(value, is(number(2)));
    }

    @Test
    public void testReturnFromNestedNamedBlockShouldWork() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("(block b1 (block b2 (block b3 (return 5 b1)) 2 ) 3)");

        assertThat(value, is(number(5)));
    }

    @Test
    public void testReturnFromPartiallyNamedBlockShouldWork() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("(block b1 (block b2 (block b3 (return 5 b2)) 2 ) 3)");

        assertThat(value, is(number(3)));
    }

    @Test
    public void testShouldLoopWhileConditionIsTrue() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("(let ((x 5) (y 0)) " +
                "(loop (> x 0) (set x (- x 1)) (set y (+ y 1)) y))");

        assertThat(value, is(number(5)));
    }

    @Test
    public void testShouldBreakLoop() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("" +
                "(let ((x 5) (y 0)) " +
                "     (loop (> x 0) " +
                "           (set x (- x 1)) " +
                "           (set y (+ y 1)) " +
                "           (if (= y 3) " +
                "               (break y))" +
                "     )" +
                ")");

        assertThat(value, is(number(3)));
    }

    @Test(expected = SemblanceRuntimeException.class)
    public void testBrakShouldFailOutsideLoop() throws Exception {
        final Interpreter interpreter = new Interpreter();

        interpreter.run("(break 1)");
    }
}
