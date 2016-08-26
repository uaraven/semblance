package net.ninjacat.semblance.concurrent.func;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.collections.LispValue;
import org.hamcrest.core.Is;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.longN;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;


public class DelayTest {

    @Test
    public void shouldDelayExecutionValue() throws Exception {
        final Interpreter semblance = AsyncFixtures.getConcurrentInterpreter();

        final long start = System.currentTimeMillis();
        final LispValue value = semblance.run(
                "(set1 x (async/run (async/delay 400) (+ 2 2)))" +
                        "(async/await x)");

        assertThat(System.currentTimeMillis() - start > 200, is(true));
        assertThat(value, Is.<LispValue>is(longN(4)));
    }

}