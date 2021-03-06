package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.LispValue;
import org.junit.Test;

import java.util.Optional;

import static net.ninjacat.semblance.utils.Values.symbol;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class RootContextTest {

    @Test
    public void testShouldLoadStandardLibrary() throws Exception {
        final RootContext context = new RootContext();

        final Optional<LispValue> defun = context.findSymbol(symbol("defun"));

        assertThat(defun.isPresent(), is(true));
        assertThat(defun.get(), instanceOf(LispCallable.class));
    }

}