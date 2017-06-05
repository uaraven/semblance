package net.ninjacat.semblance.builtin.spforms.types;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.SMap;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.RootContext;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class AsStringTest {

    @Test
    public void shouldConvertIntegerToString() throws Exception {
        final Context context = new RootContext();
        final AsString asString = new AsString();

        final LispValue lispValue = asString.applyFunction(context, list(number(10)));

        assertThat(lispValue, is(string("10")));
    }

    @Test
    public void shouldConvertDoubleToString() throws Exception {
        final Context context = new RootContext();
        final AsString asString = new AsString();

        final LispValue lispValue = asString.applyFunction(context, list(doubleN(10.2)));

        assertThat(lispValue, is(string("10.2")));
    }

    @Test
    public void shouldConvertListOfNumbersToString() throws Exception {
        final Context context = new RootContext();
        final AsString asString = new AsString();

        final LispValue lispValue = asString.applyFunction(context, list(list(doubleN(42.42), number(10))));

        assertThat(lispValue, is(list(string("42.42"), string("10"))));
    }

    @Test
    public void shouldConvertMapToString() throws Exception {
        final Context context = new RootContext();
        final AsString asString = new AsString();

        final LispValue lispValue = asString.applyFunction(context, list(SMap.newSMap(
                symbol("key1"), number(1),
                symbol("key2"), string("value")
        )));

        assertThat(lispValue, is(string("{key1 1 key2 value}")));
    }
}