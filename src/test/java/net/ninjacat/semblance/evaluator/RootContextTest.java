package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.Callable;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.smooth.utils.Option;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.symbol;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class RootContextTest {

    @Test
    public void testShouldLoadStandardLibrary() throws Exception {
        final RootContext context = new RootContext();

        final Option<LispValue> defun = context.findSymbol(symbol("defun"));

        assertThat(defun.isPresent(), is(true));
        assertThat(defun.get(), instanceOf(Callable.class));
    }

}