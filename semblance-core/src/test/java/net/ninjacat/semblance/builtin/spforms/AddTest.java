package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.builtin.spforms.arithmetic.Add;
import net.ninjacat.semblance.data.DoubleNumberAtom;
import net.ninjacat.semblance.data.LongNumberAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.data.collections.SMap;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.smooth.collections.Maps;
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
public class AddTest {

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
    public void shouldAddIntegers() throws Exception {
        final SList params = list(number(1), number(2), number(3));
        final Add add = new Add();

        final LispValue value = add.apply(context, params);

        assertThat("Result should be an integer", value, instanceOf(LongNumberAtom.class));
        assertThat("Result should be equal to 6", (Long) asNumber(value).getValue(), is(6L));
    }


    @Test
    public void shouldAddDoubles() throws Exception {
        final SList params = list(number(1), number(2.5), number(1.5));
        final Add add = new Add();

        final LispValue value = add.apply(context, params);

        assertThat("Result should be an integer", value, instanceOf(DoubleNumberAtom.class));
        assertThat("Result should be equal to 6", (Double) asNumber(value).getValue(), is(5d));
    }

    @Test(expected = TypeMismatchException.class)
    public void shouldFailToAddIntegerAndString() throws Exception {
        final SList params = list(number(1), string("2"));
        final Add add = new Add();

        add.apply(context, params);
    }

    @Test
    public void shouldConcatenateStrings() throws Exception {
        final SList params = list(string("2"), string("3"), string("-done"));
        final Add add = new Add();

        final LispValue result = add.apply(context, params);

        assertThat("Should concatenate strings", result, is(string("23-done")));
    }

    @Test(expected = TypeMismatchException.class)
    public void shouldFailToConcatenateStringAndNumber() throws Exception {
        final SList params = list(string("2"), number(3));
        final Add add = new Add();

        add.apply(context, params);
    }

    @Test
    public void shouldJoinLists() throws Exception {
        final SList params = list(list(number(1), number(2)), list(number(3), number(4)));
        final Add add = new Add();

        final LispValue result = add.apply(context, params);

        assertThat("Should join lists", result, is((LispValue) smartList(1L, 2L, 3L, 4L)));
    }

    @Test
    public void shouldJoinVectorAndList() throws Exception {
        final SList params = list(smartVector(1L, 2L), smartList(3L, 4L));
        final Add add = new Add();

        final LispValue result = add.apply(context, params);

        assertThat("Should join lists", result, is((LispValue) smartVector(1L, 2L, 3L, 4L)));
    }

    @Test(expected = TypeMismatchException.class)
    public void shouldFailToJoinListAndAtom() throws Exception {
        final SList params = list(smartVector(1L, 2L), number(3));
        final Add add = new Add();

        add.apply(context, params);
    }

    @Test
    public void shouldJoinMaps() throws Exception {
        final SList params = list(
                new SMap(Maps.<LispValue, LispValue>of(symbol(":a"), number(1)), SourceInfo.UNKNOWN),
                new SMap(Maps.<LispValue, LispValue>of(symbol(":b"), number(2)), SourceInfo.UNKNOWN)
        );
        final Add add = new Add();

        final LispValue result = add.apply(context, params);

        assertThat("Should join maps", result, is((LispValue) new SMap(
                        Maps.<LispValue, LispValue>of(symbol(":a"), number(1), symbol(":b"), number(2)), SourceInfo.UNKNOWN)
        ));
    }
}
