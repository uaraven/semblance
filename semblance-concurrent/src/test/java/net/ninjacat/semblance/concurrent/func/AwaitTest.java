package net.ninjacat.semblance.concurrent.func;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.collections.LispValue;
import org.hamcrest.core.Is;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.longN;
import static org.junit.Assert.assertThat;

@SuppressWarnings("NonBooleanMethodNameMayNotStartWithQuestion")
public class AwaitTest {

    @Test
    public void shouldReturnAsyncComputedValue() throws Exception {
        final Interpreter semblance = AsyncFixtures.getConcurrentInterpreter();

        final LispValue value = semblance.run(
                "(set1 x (async/run (+ 2 2)))" +
                        "(async/await x)");

        assertThat(value, Is.<LispValue>is(longN(4)));
    }

}