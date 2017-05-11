package net.ninjacat.semblance.java;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.errors.runtime.FunctionExpectedException;
import org.junit.Test;

import java.util.function.Function;

import static net.ninjacat.semblance.utils.Values.number;
import static net.ninjacat.semblance.utils.Values.smartList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;

public class FunctionFactoryTest {

    @Test
    public void shouldReturnFunction() throws Exception {
        final Interpreter interpreter = new Interpreter();

        interpreter.runHere("(defun abs (x) (if (>= x 0) x (- x)))");

        final FunctionFactory factory = new FunctionFactory(interpreter);

        final Function<SList, LispValue> func = factory.getFunction("abs");
        final LispValue value = func.apply(smartList(-10));

        assertThat(value, is(number(10)));

    }

    @Test(expected = FunctionExpectedException.class)
    public void shouldFailtoCallNonexistingFunction() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final FunctionFactory factory = new FunctionFactory(interpreter);

        final Function<SList, LispValue> func = factory.getFunction("abs");
        func.apply(smartList(-10));

    }
}