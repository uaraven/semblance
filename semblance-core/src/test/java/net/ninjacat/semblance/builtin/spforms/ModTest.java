package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.builtin.spforms.arithmetic.Mod;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.LongNumberAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.errors.runtime.UnsupportedOperationException;
import net.ninjacat.semblance.evaluator.Context;
import org.junit.Before;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@SuppressWarnings("NonBooleanMethodNameMayNotStartWithQuestion")
public class ModTest {

    private Context context;

    @Before
    public void setUp() throws Exception {
        context = mock(Context.class);
        when(context.evaluateList(any(LispCollection.class)))
                .thenAnswer(invocationOnMock -> invocationOnMock.getArguments()[0]);
    }

    @Test
    public void shouldCalculateModuloForTwoInts() throws Exception {
        final SList params = list(number(5), number(2));
        final Mod mod = new Mod();

        final LispValue value = mod.apply(context, params);

        assertThat("Result should be an integer", value, instanceOf(LongNumberAtom.class));
        assertThat("Result should be equal to 1", asNumber(value).getValue(), is(1L));
    }

    @Test
    public void shouldCalculateModuloForMultipleInts() throws Exception {
        final SList params = list(number(17), number(8), number(7));
        final Mod mod = new Mod();

        final LispValue value = mod.apply(context, params);

        assertThat("Result should be an integer", value, instanceOf(LongNumberAtom.class));
        assertThat("Result should be equal to 1", asNumber(value).getValue(), is(1L));
    }


    @Test(expected = UnsupportedOperationException.class)
    public void shouldNotCalculateModuleForDoubles() throws Exception {
        final SList params = list(number(1), number(2.5), number(1.5));
        final Mod mod = new Mod();

        mod.apply(context, params);
    }

    @Test(expected = TypeMismatchException.class)
    public void shouldFailToModIntegerAndString() throws Exception {
        final SList params = list(number(1), string("2"));
        final Mod mod = new Mod();

        mod.apply(context, params);
    }

    @Test(expected = TypeMismatchException.class)
    public void shouldFailForStrings() throws Exception {
        final SList params = list(string("2"), string("3"), string("-done"));
        final Mod mod = new Mod();

        mod.apply(context, params);
    }
}
