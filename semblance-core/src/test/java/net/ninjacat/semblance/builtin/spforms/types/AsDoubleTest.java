package net.ninjacat.semblance.builtin.spforms.types;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.RootContext;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class AsDoubleTest {

    @Test
    public void shouldConvertDoublesToDouble() throws Exception {
        final Context context = new RootContext();
        final AsDouble asDouble = new AsDouble();

        final LispValue lispValue = asDouble.applyFunction(context, list(doubleN(10.5)));

        assertThat(lispValue, is(number(10.5)));
    }

    @Test
    public void shouldConvertIntToDouble() throws Exception {
        final Context context = new RootContext();
        final AsDouble asDouble = new AsDouble();

        final LispValue lispValue = asDouble.applyFunction(context, list(number(10L)));

        assertThat(lispValue, is(number(10.0)));
    }

    @Test
    public void shouldConvertListOfNumbersToListOfDoubles() throws Exception {
        final Context context = new RootContext();
        final AsDouble asDouble = new AsDouble();

        final LispValue lispValue = asDouble.applyFunction(context, list(list(doubleN(42.42), number(10))));

        assertThat(lispValue, is(list(number(42.42), number(10.0))));
    }


    @Test(expected = TypeMismatchException.class)
    public void shouldFailToConvertStringToDouble() throws Exception {
        final Context context = new RootContext();
        final AsDouble asDouble = new AsDouble();

        asDouble.applyFunction(context, list(string("42.0")));
    }

}