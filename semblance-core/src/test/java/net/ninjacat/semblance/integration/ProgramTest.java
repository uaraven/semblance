package net.ninjacat.semblance.integration;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.Macro;
import net.ninjacat.semblance.errors.runtime.FunctionExpectedException;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
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
    public void testVarShouldBindSequentally() throws Exception {
        final Interpreter interpreter = new Interpreter();
        final LispValue value = interpreter.run(
                "(set" +
                        "(x 1)" +
                        "(y (+ x 1))" +
                        "(z (+ y 1))" +
                        ")");

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


    @Test
    public void testFunctionShouldWork() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("(set (f (fn (x y) (+ x y)))) (f 2 1)");

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
    public void testLetShouldAssignValuesToVars() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("(let ((x 5) (y 0)) " +
                "(+ x y))");

        assertThat(value, is(number(5)));
    }

    @Test(expected = TypeMismatchException.class)
    public void testLetShouldFailWhenVarsNotInList() throws Exception {
        final Interpreter interpreter = new Interpreter();

        interpreter.run("(let (x 5) x)");
    }

    @Test
    public void testShouldLoopWhileConditionIsTrue() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("(let ((x 5) (y 0)) " +
                "(loop (> x 0) (set1 x (- x 1)) (set1 y (+ y 1)) y))");

        assertThat(value, is(number(5)));
    }

    @Test
    public void testShouldBreakLoop() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(let ((x 5) (y 0)) " +
                        "     (loop (> x 0) " +
                        "           (set1 x (- x 1)) " +
                        "           (set1 y (+ y 1)) " +
                        "           (if (= y 3) " +
                        "               (break y))" +
                        "     )" +
                        ")");

        assertThat(value, is(number(3)));
    }


    @Test
    public void testShouldContinueLoop() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(let ((x 5) (y 0)) " +
                        "     (loop (> x 0) " +
                        "           (set1 x (- x 1)) " +
                        "           (if (= x 3) " +
                        "               (recur))" +
                        "           (set1 y (+ y 1)) " +
                        "     )" +
                        ")");

        assertThat(value, is(number(4)));
    }

    @Test(expected = SemblanceRuntimeException.class)
    public void testBrakShouldFailOutsideLoop() throws Exception {
        final Interpreter interpreter = new Interpreter();

        interpreter.run("(break 1)");
    }


    @Test
    public void testShouldEvaluateLambdaInPlace() throws Exception {

        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "((fn (x) (* x x)) 3)");

        assertThat(value, is(number(9)));
    }

    @Test
    public void testShouldEvaluateLambdaWithItInPlace() throws Exception {

        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "((fn* (* it it)) 3)");

        assertThat(value, is(number(9)));
    }

    @Test
    public void testShouldEvaluateRecursiveCalls() throws Exception {

        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(defun rec (x) (if (= 0 x) \"Tada\" (recur (- x 1)))) (rec 100)"
        );

        assertThat(value, is(string("Tada")));
    }

    @Test
    public void testShouldEvaluateHugeNumberOfRecursiveCalls() throws Exception {

        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(defun rec (x) (if (= 0 x) \"Tada\" (recur (- x 1)))) (rec 1000)"
        );

        assertThat(value, is(string("Tada")));
    }

    @Test
    public void testUseShouldMakeNamespaceBindingsAvailable() throws Exception {

        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(use list (find [1 2 3] 3))"
        );

        assertThat(value, is(number(2)));
    }


    @Test(expected = FunctionExpectedException.class)
    public void testShouldFailWhenCallingUnknownFunction() throws Exception {

        final Interpreter interpreter = new Interpreter();

        interpreter.run("(unknown-function 1 2 3)");
    }
}
