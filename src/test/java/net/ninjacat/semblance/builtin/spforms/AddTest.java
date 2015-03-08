package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.builtin.spforms.arithmetic.Add;
import net.ninjacat.semblance.data.*;
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
}
