package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.InterpretedFunction;
import net.ninjacat.semblance.evaluator.Context;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;

public class FnTest {

    @Test
    public void testShouldCreateAFunction() throws Exception {
        final Fn fn = new Fn();

        final Context context = mock(Context.class);

        final LispValue value = fn.apply(
                context,
                list(
                        vector(symbol("x"), symbol("y")),
                        list(symbol("+"), symbol("x"), symbol("y")))
        );

        assertThat(value, instanceOf(InterpretedFunction.class));
    }
}