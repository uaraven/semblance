package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.mockito.Mockito.*;

public class VarTest {

    @Test
    public void shouldBindValueToSymbolInContext() throws Exception {
        final Context context = mock(Context.class);
        final SymbolAtom name = symbol("x");
        final LispValue value = number(42);
        when(context.evaluate(value)).thenReturn(value);

        final Var varExpression = new Var();

        varExpression.apply(context, list(list(name, value)));

        verify(context).bind(name, value);
    }
}
