package net.ninjacat.semblance.integration;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.NilCollection;
import org.hamcrest.core.IsSame;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Standard library integration tests
 */
public class StandardLibraryTest {

    @Test
    public void testLetShouldWork() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("(let ((x 10) (y 32)) (+ x y)) ");

        assertThat(value, is(number(42)));
        assertThat(interpreter.getRootContext().findSymbol(symbol("x")).isPresent(), is(false));
        assertThat(interpreter.getRootContext().findSymbol(symbol("y")).isPresent(), is(false));
    }

    @Test
    public void testReturnFromShouldWorkForFunction() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("(defun fun () (block fff (return-from fun 42)) 15) (fun)");

        assertThat(value, is(number(42)));
    }

    @Test
    public void testUnnamedReturnShouldWorkForNestedBlockInFunction() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("(defun fun () (block fff (return 42)) 15) (fun)");

        assertThat(value, is(number(15)));
    }

    @Test
    public void testReturnFromWithoutValueShouldWork() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("(defun fun () (block fff (return-from fun)) 15) (fun)");

        assertThat(value, IsSame.sameInstance((LispValue) NilCollection.INSTANCE));
    }


    @Test
    public void testReturnFromNestedNamedBlockShouldWork() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("(block b1 (block b2 (block b3 (return-from b1 5)) 2 ) 3)");

        assertThat(value, is(number(5)));
    }

    @Test
    public void testCarShouldReturnListHead() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(car '(1 2 3 4 5))");

        assertThat(value, is(number(1)));
    }

    @Test
    public void testCdrShouldReturnListTail() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(cdr '(1 2 3 4 5))");

        assertThat(value, is((LispValue) smartList(2L, 3L, 4L, 5L)));
    }

    @Test
    public void testShouldLoopOverList() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(set res) (do-list [1 2 3] x (set* (res (res :prepend x)))) res");

        assertThat(value, is((LispValue) smartList(3L, 2L, 1L)));

    }


    @Test
    public void testShouldLoopNumberOfTimes() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(set res) (do-times x 3 (set* (res (res :append x)))) res");

        assertThat(value, is((LispValue) smartList(0L, 1L, 2L)));

    }
}
