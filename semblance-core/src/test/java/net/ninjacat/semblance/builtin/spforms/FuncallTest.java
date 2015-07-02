package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.RootContext;
import net.ninjacat.semblance.utils.IOUtils;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.number;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class FuncallTest {

    @Test
    public void testShouldCallFunctionByBinding() throws Exception {

        final RootContext context = new RootContext();

        final LispValue value = context.evaluateProgram(IOUtils.asStream(
                "(defun mult2 (x) (* 2 x))\n" +
                        "(funcall mult2 (21))"
        ));


        assertThat(value, is(number(42)));
    }
}