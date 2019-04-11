package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.errors.compile.ParsingException;
import org.hamcrest.Matchers;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.number;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;

public class ResolveTest {

    @Test
    public void shouldResolveVariable() throws ParsingException {
        final Interpreter interpreter = new Interpreter();
        final LispValue value = interpreter.run(
                "(set1 x 122)(resolve x)");

        assertThat(value, is(number(122)));
    }

    @Test
    public void shouldResolveFunction() throws ParsingException {
        final Interpreter interpreter = new Interpreter();
        final LispValue value = interpreter.run(
                "(resolve set1)");

        assertThat(value, instanceOf(LispCallable.class));
    }

    @Test
    public void shouldResolveValue() throws ParsingException {
        final Interpreter interpreter = new Interpreter();
        final LispValue value = interpreter.run(
                "(resolve 12.2)");

        assertThat(value, is(number(12.2)));
    }
}
