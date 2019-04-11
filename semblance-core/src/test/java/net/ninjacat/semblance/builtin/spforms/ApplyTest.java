package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.errors.runtime.UnboundSymbolException;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.number;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.*;

public class ApplyTest {

    @Test
    public void shouldApplyFunction() throws ParsingException {
        final Interpreter interpreter = new Interpreter();
        final LispValue value = interpreter.run(
                "(apply + (1 2))");

        assertThat(value, is(number(3)));
    }

    @Test(expected = TypeMismatchException.class)
    public void testSecondParameterMustBeList() throws ParsingException {
        final Interpreter interpreter = new Interpreter();
        interpreter.run("(apply + 2)");

        fail("Should have failed with runtime error");
    }

    @Test(expected = UnboundSymbolException.class)
    public void testFirstParameterMustBeFunction() throws ParsingException {
        final Interpreter interpreter = new Interpreter();
        interpreter.run("(apply non-func (1 2))");

        fail("Should have failed with runtime error");
    }
}