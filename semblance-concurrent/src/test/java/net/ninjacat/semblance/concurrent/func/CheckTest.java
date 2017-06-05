package net.ninjacat.semblance.concurrent.func;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.utils.Values;
import org.hamcrest.core.Is;
import org.junit.Test;

import static org.junit.Assert.assertThat;

public class CheckTest {

    @Test
    public void shouldReturnCorrectlyCheckNonComputedFuture() throws Exception {
        final Interpreter semblance = AsyncFixtures.getConcurrentInterpreter();

        final LispValue value = semblance.run(
                "(set1 x (async/run (async/delay 400) (+ 2 2)))" +
                        "(async/check x)");

        assertThat(value, Is.<LispValue>is(Values.F));
    }


    @Test
    public void shouldReturnCorrectlyCheckComputedFuture() throws Exception {
        final Interpreter semblance = AsyncFixtures.getConcurrentInterpreter();

        final LispValue value = semblance.run(
                "(set1 x (async/run (async/delay 200) (+ 2 2)))" +
                        "(async/delay 400)" +
                        "(async/check x)");
        assertThat(value, Is.<LispValue>is(Values.T));
    }

}