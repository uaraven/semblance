package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.evaluator.RootContext;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.list;
import static net.ninjacat.semblance.utils.Values.symbol;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

@SuppressWarnings({"NonBooleanMethodNameMayNotStartWithQuestion", "DuplicateStringLiteralInspection"})
public class QuoteTest {

    private static final SymbolAtom SYMBOL = symbol("symbol");
    private static final SList LIST = list(symbol("a1"), symbol("a2"));

    @Test
    public void shouldReturnSymbolVerbatim() throws Exception {
        final SList program = list(symbol("quote"), SYMBOL);

        final LispValue result = new RootContext().evaluate(program);

        assertThat((SymbolAtom) result, is(SYMBOL));
    }

    @Test
    public void shouldReturnListVerbatim() throws Exception {
        final SList program = list(symbol("quote"), LIST);

        final LispValue result = new RootContext().evaluate(program);

        assertThat((SList) result, is(LIST));
    }
}