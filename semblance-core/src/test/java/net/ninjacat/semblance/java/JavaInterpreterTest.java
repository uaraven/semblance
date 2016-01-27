package net.ninjacat.semblance.java;

import net.ninjacat.semblance.Interpreter;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.symbol;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;

public class JavaInterpreterTest {

    @Test
    public void testShouldHaveJavaNamespace() throws Exception {
        final Interpreter interpreter = createJavaInterpreter();

        assertThat(interpreter.getRootContext().findNamespace(symbol("java")), notNullValue());
    }

    private Interpreter createJavaInterpreter() {
        return new Interpreter(new JavaBridge());
    }
}
