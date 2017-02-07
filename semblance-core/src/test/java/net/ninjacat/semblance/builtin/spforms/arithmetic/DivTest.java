package net.ninjacat.semblance.builtin.spforms.arithmetic;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.RootContext;
import net.ninjacat.semblance.utils.Values;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.number;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;


public class DivTest {
    @Test
    public void shouldDivideSimpleValues() throws Exception {
        final RootContext context = new RootContext();
        final Div div = new Div();

        final LispValue result = div.applyFunction(context, Values.list(number(10), number(2)));

        assertThat(result, is(number(5)));
    }

    @Test(expected = ArithmeticException.class)
    public void shouldNotDivideByZero() throws Exception {
        final RootContext context = new RootContext();
        final Div div = new Div();

        div.applyFunction(context, Values.list(number(10), number(0)));

    }
}