package net.ninjacat.semblance.builtin.spforms.types;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.RootContext;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class AsIntTest {

    @Test
    public void shouldConvertDoublesToInt() throws Exception {
        final Context context = new RootContext();
        final AsInt asInt = new AsInt();

        final LispValue lispValue = asInt.applyFunction(context, list(doubleN(10.5)));

        assertThat(lispValue, is(number(10)));
    }

    @Test
    public void shouldConvertIntToInt() throws Exception {
        final Context context = new RootContext();
        final AsInt asInt = new AsInt();

        final LispValue lispValue = asInt.applyFunction(context, list(number(10L)));

        assertThat(lispValue, is(number(10L)));
    }

    @Test
    public void shouldConvertListOfNumbersToListOfInts() throws Exception {
        final Context context = new RootContext();
        final AsInt asInt = new AsInt();

        final LispValue lispValue = asInt.applyFunction(context, list(list(doubleN(42.42), number(10))));

        assertThat(lispValue, is(list(number(42), number(10))));
    }


    @Test(expected = TypeMismatchException.class)
    public void shouldFailToConvertStringToInt() throws Exception {
        final Context context = new RootContext();
        final AsInt asInt = new AsInt();

        asInt.applyFunction(context, list(string("42")));
    }

}