package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Values;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.number;
import static net.ninjacat.semblance.utils.Values.symbol;
import static org.mockito.Mockito.*;

public class VarTest {

    @Test
    public void shouldBindValueToSymbolInContext() throws Exception {
        Context context = mock(Context.class);
        SymbolAtom name = symbol("x");
        LispValue value = number(42);
        when(context.evaluate(value)).thenReturn(value);

        Var var = new Var();

        var.apply(context, Values.list(name, value));

        verify(context).bind(name, value);
    }
}
