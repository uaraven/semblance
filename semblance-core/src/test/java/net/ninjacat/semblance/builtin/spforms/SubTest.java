package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.builtin.spforms.arithmetic.Sub;
import net.ninjacat.semblance.data.DoubleNumberAtom;
import net.ninjacat.semblance.data.LongNumberAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.data.collections.SMap;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Maps;
import org.junit.Before;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@SuppressWarnings({"NonBooleanMethodNameMayNotStartWithQuestion", "DuplicateStringLiteralInspection"})
public class SubTest {

    private Context context;

    @Before
    public void setUp() throws Exception {
        context = mock(Context.class);
        when(context.evaluateList(any(LispCollection.class))).thenAnswer(
                invocationOnMock -> invocationOnMock.getArguments()[0]);
    }

    @Test
    public void shouldAddIntegers() throws Exception {
        final SList params = list(number(6), number(3), number(10));
        final Sub sub = new Sub();

        final LispValue value = sub.apply(context, params);

        assertThat("Result should be an integer", value, instanceOf(LongNumberAtom.class));
        assertThat("Result should be equal to -7", asNumber(value).getValue(), is(-7L));
    }


    @Test
    public void shouldAddDoubles() throws Exception {
        final SList params = list(number(5.5), number(2.5), number(0.5));
        final Sub sub = new Sub();

        final LispValue value = sub.apply(context, params);

        assertThat("Result should be a double", value, instanceOf(DoubleNumberAtom.class));
        assertThat("Result should be equal to 3.5", asNumber(value).getValue(), is(2.5d));
    }

    @Test(expected = TypeMismatchException.class)
    public void shouldFailToAddIntegerAndString() throws Exception {
        final SList params = list(number(1), string("2"));
        final Sub sub = new Sub();

        sub.apply(context, params);
    }

    @Test
    public void shouldSubtractLists() throws Exception {
        final SList params = list(smartList(1L, 2L, 3L), smartList(1L, 3L));
        final Sub sub = new Sub();

        final LispValue value = sub.apply(context, params);

        assertThat("Should subtract lists", value, is(smartList(2L)));
    }

    @Test
    public void shouldSubtractListFromVector() throws Exception {
        final SList params = list(smartVector(1L, 2L, 3L), smartList(1L, 3L));
        final Sub sub = new Sub();

        final LispValue value = sub.apply(context, params);

        assertThat("Should subtract lists", value, is(smartVector(2L)));
    }

    @Test
    public void shouldSubtractMaps() throws Exception {
        final SList params = list(new SMap(Maps.<LispValue, LispValue>of(
                symbol(":a"), number(1),
                symbol(":b"), number(2)), SourceInfo.UNKNOWN),
                new SMap(Maps.<LispValue, LispValue>of(
                        symbol(":b"), number(1),
                        symbol(":c"), number(2)), SourceInfo.UNKNOWN));
        final Sub sub = new Sub();

        final LispValue value = sub.apply(context, params);

        assertThat("Should subtract lists", value, is(new SMap(Maps.<LispValue, LispValue>of(symbol(":a"), number(1)), SourceInfo.UNKNOWN)));
    }

    @Test
    public void shouldNegatePositiveLong() throws Exception {
        final SList params = list(number(42));
        final Sub sub = new Sub();

        final LispValue value = sub.apply(context, params);

        assertThat("Should negate long", value, is(number(-42)));
    }

    @Test
    public void shouldNegateNegativeLong() throws Exception {
        final SList params = list(number(-42));
        final Sub sub = new Sub();

        final LispValue value = sub.apply(context, params);

        assertThat("Should negate long", value, is(number(42)));
    }

    @Test
    public void shouldNegatePositiveDouble() throws Exception {
        final SList params = list(number(42.42));
        final Sub sub = new Sub();

        final LispValue value = sub.apply(context, params);

        assertThat("Should negate long", value, is(number(-42.42)));
    }

    @Test
    public void shouldNegateNegativeDouble() throws Exception {
        final SList params = list(number(-42.42));
        final Sub sub = new Sub();

        final LispValue value = sub.apply(context, params);

        assertThat("Should negate long", value, is(number(42.42)));
    }

    @Test
    public void shouldNegatePositiveBigInt() throws Exception {
        final SList params = list(number("42"));
        final Sub sub = new Sub();

        final LispValue value = sub.apply(context, params);

        assertThat("Should negate long", value, is(number("-42")));
    }

    @Test
    public void shouldNegateNegativeBigInt() throws Exception {
        final SList params = list(number("-42"));
        final Sub sub = new Sub();

        final LispValue value = sub.apply(context, params);

        assertThat("Should negate long", value, is(number("42")));
    }
}
