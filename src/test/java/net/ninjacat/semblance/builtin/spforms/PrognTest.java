package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NilCollection;
import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.RootContext;
import org.junit.Before;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class PrognTest {

    public static final SymbolAtom SYMBOL = symbol(":final");
    private Context context;

    @Before
    public void setUp() throws Exception {
        context = mock(Context.class);

        when(context.evaluate(any(SymbolAtom.class))).thenAnswer(new Answer<SymbolAtom>() {
            @Override
            public SymbolAtom answer(final InvocationOnMock invocationOnMock) throws Throwable {
                return (SymbolAtom) invocationOnMock.getArguments()[0];
            }
        });
        when(context.evaluate(any(SList.class))).thenReturn(NilCollection.INSTANCE);

    }

    @Test
    public void shouldEvaluateExpressions() throws Exception {
        final SList params = list(
                list(symbol("+"), number(1), number(1)),
                SYMBOL
        );

        final Progn progn = new Progn();

        final LispValue value = progn.apply(new RootContext(), params);

        assertThat((SymbolAtom) value, is(SYMBOL));
    }
}