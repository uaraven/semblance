package net.ninjacat.semblance.concurrent.func;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import org.hamcrest.core.Is;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.longN;
import static org.junit.Assert.assertThat;

@SuppressWarnings("NonBooleanMethodNameMayNotStartWithQuestion")
public class GoFuncTest {

    @Test
    public void shouldReturnResultOfAsyncComputedFunction() throws Exception {
        final Interpreter semblance = AsyncFixtures.getConcurrentInterpreter();

        final LispValue value = semblance.run(
                "(set1 x 2)" +
                        "(set1 x (async/go + (2 2)))" +
                        "(async/await x)");
        assertThat(value, Is.<LispValue>is(longN(4)));
    }

    @Test
    public void shouldReturnResultOfAsyncComputedLambda() throws Exception {
        final Interpreter semblance = AsyncFixtures.getConcurrentInterpreter();

        final LispValue value = semblance.run(
                "(set1 x (async/go (fn (x y) (+ x y)) (2 2)))" +
                        "(async/await x)");
        assertThat(value, Is.<LispValue>is(longN(4)));
    }
}