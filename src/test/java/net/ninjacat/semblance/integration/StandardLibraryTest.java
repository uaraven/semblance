package net.ninjacat.semblance.integration;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NilCollection;
import org.hamcrest.core.IsSame;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.number;
import static net.ninjacat.semblance.utils.Values.symbol;
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

}
