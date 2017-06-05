package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.NilCollection;
import org.hamcrest.Matchers;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.number;
import static org.junit.Assert.assertThat;

public class SelectTest {

    @Test
    public void shouldReturnFirstNonNilValue() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(let ((x 1) (y nil))" +
                        "(select "
                        + "(x)"
                        + "(y)" +
                        "))");

        assertThat(value, Matchers.is(number(1)));
    }

    @Test
    public void shouldReturnFirstNonNilValue2() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(let ((x 1) (y nil))" +
                        "(select "
                        + "(y)"
                        + "(x)" +
                        "))");

        assertThat(value, Matchers.is(number(1)));
    }


    @Test
    public void shouldReturnNilValue() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(let ((x nil) (y nil))" +
                        "(select "
                        + "(y)"
                        + "(x)" +
                        "))");

        assertThat(value, Matchers.<LispValue>is(NilCollection.INSTANCE));
    }
}