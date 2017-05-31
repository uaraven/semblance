package net.ninjacat.semblance.builtin.spforms.types;


import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.evaluator.Context;
import org.junit.Before;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;

public class ParseNumberTest {

    private Context context;

    @Before
    public void setUp() throws Exception {
        context = mock(Context.class);
    }

    @Test
    public void shouldParseSingleInteger() throws Exception {
        final ParseNumber parseNumber = new ParseNumber();

        final LispValue result = parseNumber.applyFunction(context, list(string("42")));

        assertThat(result, is(number(42)));
    }


    @Test
    public void shouldParseSingleDouble() throws Exception {
        final ParseNumber parseNumber = new ParseNumber();

        final LispValue result = parseNumber.applyFunction(context, list(string("42.42")));

        assertThat(result, is(number(42.42)));
    }

    @Test
    public void shouldParseSingleDoubleExp() throws Exception {
        final ParseNumber parseNumber = new ParseNumber();

        final LispValue result = parseNumber.applyFunction(context, list(string("42.0e42")));

        assertThat(result, is(number(42e42)));
    }

    @Test
    public void shouldParseSingleBigInt() throws Exception {
        final ParseNumber parseNumber = new ParseNumber();

        final LispValue result = parseNumber.applyFunction(context, list(string("4242424242424242424242424242424242424242")));

        assertThat(result, is(number("4242424242424242424242424242424242424242")));
    }

    @Test
    public void shouldParseListOfNumbers() throws Exception {
        final ParseNumber parseNumber = new ParseNumber();

        final LispValue result = parseNumber.applyFunction(context, list(vector(
                string("42"), string("42.42"), string("4242424242424242424242424242424242424242"))));

        assertThat(result, is(vector(number(42), number(42.42), number("4242424242424242424242424242424242424242"))));
    }

    @Test(expected = TypeMismatchException.class)
    public void shouldFailToParseNonString() throws Exception {
        final ParseNumber parseNumber = new ParseNumber();

        parseNumber.applyFunction(context, list(number(42)));
    }


    @Test(expected = SemblanceRuntimeException.class)
    public void shouldFailToParseNonNumber() throws Exception {
        final ParseNumber parseNumber = new ParseNumber();

        parseNumber.applyFunction(context, list(string("forty-two")));
    }
}