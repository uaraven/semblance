package net.ninjacat.semblance.integration;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
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

}
