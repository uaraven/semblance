package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.RootContext;
import net.ninjacat.semblance.utils.IOUtils;
import net.ninjacat.smooth.utils.Option;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.number;
import static net.ninjacat.semblance.utils.Values.symbol;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class NamespaceTest {

    @Test
    public void testShouldBindVarInNamespace() throws Exception {
        final RootContext context = new RootContext();

        context.evaluateHere(IOUtils.asStream(
                "(namespace math (set (pi 3.14))) math/pi"));

        final Option<net.ninjacat.semblance.evaluator.Namespace> namespace = context.getNamespace(symbol("math"));

        assertThat(namespace.isPresent(), is(true));

        assertThat(namespace.get().findSymbol(symbol("pi")).isPresent(), is(true));
        assertThat(namespace.get().findSymbol(symbol("pi")).get(), is(number(3.14)));
    }


    @Test
    public void testShouldBindFunctionInNamespace() throws Exception {
        final RootContext context = new RootContext();

        final LispValue value = context.evaluateHere(IOUtils.asStream(
                "(namespace math (defun pi () 3.14)) (math/pi)"));

        final Option<net.ninjacat.semblance.evaluator.Namespace> namespace = context.getNamespace(symbol("math"));

        assertThat(value, is(number(3.14)));
        assertThat(namespace.isPresent(), is(true));

        assertThat(namespace.get().findSymbol(symbol("pi")).isPresent(), is(true));
        assertThat(namespace.get().findSymbol(symbol("pi")).get(), instanceOf(LispCallable.class));
    }
}