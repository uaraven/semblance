package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.utils.Values;
import org.junit.Test;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class DefaultContextTest {

    public static final SymbolAtom KEYWORD_SYMBOL = Values.symbol(":keyword");

    @Test
    public void keywordShouldEvaluateToItself() throws Exception {
        final DefaultContext context = new DefaultContext("", null);

        final LispValue value = context.evaluate(KEYWORD_SYMBOL);

        assertThat("Keyword should evaluate to itself", (SymbolAtom) value, is(KEYWORD_SYMBOL));
    }
}