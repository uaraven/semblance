package net.ninjacat.semblance.integration;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.reader.Reader;
import net.ninjacat.semblance.utils.Values;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Created on 03/03/15.
 */
public class ProgramTest {

    @Test
    public void shouldDoSimpleArithmetic() throws Exception {
        Reader reader = new Reader();

        LispValue value = reader.run("(+ 2 (- 4 2))");

        assertThat(value, is(Values.number(4)));
    }
}
