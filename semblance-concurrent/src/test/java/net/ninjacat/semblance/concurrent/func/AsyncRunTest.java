package net.ninjacat.semblance.concurrent.func;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.concurrent.data.SFuture;
import net.ninjacat.semblance.data.collections.LispValue;
import org.hamcrest.core.Is;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.longN;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.junit.Assert.assertThat;

@SuppressWarnings("NonBooleanMethodNameMayNotStartWithQuestion")
public class AsyncRunTest {

    @Test
    public void shouldCreateOpaqueValue() throws Exception {
        final Interpreter semblance = AsyncFixtures.getConcurrentInterpreter();

        final LispValue value = semblance.run("(async/run (+ 2 2))");

        assertThat(value, instanceOf(SFuture.class));
        assertThat(((SFuture) value).getValue().getResult().getValue(), Is.<LispValue>is(longN(4)));
    }

}