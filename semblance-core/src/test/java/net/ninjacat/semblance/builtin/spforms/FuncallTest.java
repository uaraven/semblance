package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.utils.IOUtils;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.number;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class FuncallTest {

    @Test
    public void testShouldCallFunctionByBinding() throws Exception {

        final Interpreter context = new Interpreter();

        final LispValue value = context.run(IOUtils.asStream(
                "(defun mult2 (x) (* 2 x))\n" +
                        "(funcall mult2 (21))"
        ));


        assertThat(value, is(number(42)));
    }
}