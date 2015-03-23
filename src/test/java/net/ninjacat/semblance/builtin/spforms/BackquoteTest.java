package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.RootContext;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;


public class BackQuoteTest {

    @Test
    public void testShouldUnquoteProperly() throws Exception {
        final BackQuote backQuote = new BackQuote();

        final Context ctx = new RootContext();
        ctx.bind(symbol("p1"), symbol("+"));

        final LispValue result = backQuote.apply(ctx, list(symbol(",p1"), number(1), number(2)));

        assertThat(result, instanceOf(LispCollection.class));
        assertThat((SList) result, is(list(symbol("+"), number(1), number(2))));
    }


    @Test
    public void testShouldUnquoteAndExpandProperly() throws Exception {
        final BackQuote backQuote = new BackQuote();

        final Context ctx = new RootContext();
        ctx.bind(symbol("p1"), symbol("+"));
        ctx.bind(symbol("p2"), list(number(1), number(2)));

        final LispValue result = backQuote.apply(ctx, list(symbol(",p1"), symbol(",@p2")));

        assertThat(result, instanceOf(LispCollection.class));
        assertThat((SList) result, is(list(symbol("+"), number(1), number(2))));
    }
}