package net.ninjacat.semblance.builtin.spforms.arithmetic;

import net.ninjacat.semblance.data.DoubleNumberAtom;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.LongNumberAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.errors.runtime.ParameterException;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.evaluator.Context;
import org.junit.Before;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@SuppressWarnings("NonBooleanMethodNameMayNotStartWithQuestion")
public class MulTest {

    private Context context;

    @Before
    public void setUp() throws Exception {
        context = mock(Context.class);
        when(context.evaluateList(any(LispCollection.class))).thenAnswer(new Answer<LispCollection>() {
            @Override
            public LispCollection answer(final InvocationOnMock invocationOnMock) throws Throwable {
                return (LispCollection) invocationOnMock.getArguments()[0];
            }
        });
    }

    @Test
    public void shouldMultiplyIntegers() throws Exception {
        final SList params = list(number(1), number(2), number(3));
        final Mul mul = new Mul();

        final LispValue value = mul.apply(context, params);

        assertThat("Result should be an integer", value, instanceOf(LongNumberAtom.class));
        assertThat("Result should be equal to 6", (Long) asNumber(value).getValue(), is(6L));
    }


    @Test
    public void shouldMultiplyDoubles() throws Exception {
        final SList params = list(number(5.5), number(2.5), number(0.5));
        final Mul mul = new Mul();

        final LispValue value = mul.apply(context, params);

        assertThat("Result should be a double", value, instanceOf(DoubleNumberAtom.class));
        assertThat("Result should be equal to 6.875", (Double) asNumber(value).getValue(), is(5.5d * 2.5 * 0.5));
    }

    @Test(expected = TypeMismatchException.class)
    public void shouldFailToAddIntegerAndString() throws Exception {
        final SList params = list(number(1), string("2"));
        final Mul mul = new Mul();

        mul.apply(context, params);
    }

    @Test
    public void shouldPerformScalarMultiplicationOnList() throws Exception {
        final SList params = list(list(number(1.5), number(2.5)), number(2.0));
        final Mul mul = new Mul();

        final LispValue value = mul.apply(context, params);

        assertThat("Should perform scalar multiplication", value, is((LispValue) smartList(3.0d, 5.0d)));
    }

    @Test
    public void shouldCalculateCartesianProdiuct() throws Exception {
        final SList params = list(smartList(1L, 2L), smartList("a", "b"));
        final Mul mul = new Mul();

        final LispValue value = mul.apply(context, params);

        assertThat("Should calculate cartesian product", value, is((LispValue)
                list(smartList(1L, "a"), smartList(1L, "b"),
                        smartList(2L, "a"), smartList(2L, "b"))));
    }

    @Test(expected = ParameterException.class)
    public void shouldFailToProcessListWithExtraParameters() throws Exception {
        final SList params = list(list(number(1.5), number(2.5)), number(2.0), number(1));
        final Mul mul = new Mul();

        mul.apply(context, params);
    }

    @Test(expected = TypeMismatchException.class)
    public void shouldPerformScalarMultiplicationOnListAndString() throws Exception {
        final SList params = list(list(number(1.5), number(2.5)), string("2.0"));
        final Mul mul = new Mul();

        mul.apply(context, params);
    }
}