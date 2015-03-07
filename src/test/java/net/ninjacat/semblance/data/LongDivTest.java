package net.ninjacat.semblance.data;

import org.junit.Test;

import java.math.BigInteger;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

/**
 * Created on 03/03/15.
 */
@SuppressWarnings("DuplicateStringLiteralInspection")
public class LongDivTest {

    @SuppressWarnings("unchecked")
    @Test
    public void testDivLongs() throws Exception {
        final NumberAtom n1 = new LongNumberAtom(42);
        final NumberAtom n2 = new LongNumberAtom(42);
        final NumberAtom result = n1.div(n2);

        assertThat("Should divide two longs", result, instanceOf(LongNumberAtom.class));
        assertThat("Should divide two longs", (Long) result.getValue(), is(1L));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testSubLongAndBigWithSimplification() throws Exception {
        final NumberAtom n1 = new LongNumberAtom(42);
        final NumberAtom n2 = new BigIntegerNumberAtom(new BigInteger("42"));
        final NumberAtom result = n1.div(n2);

        assertThat("Result should be Long", result, instanceOf(LongNumberAtom.class));
        assertThat("Should divide long and Bigint and convert result to long", (Long) result.getValue(), is(1L));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testDivLongAndDouble() throws Exception {
        final NumberAtom n1 = new LongNumberAtom(21);
        final NumberAtom n2 = new DoubleNumberAtom(42);
        final NumberAtom result = n1.div(n2);

        assertThat("Result should be double", result, instanceOf(DoubleNumberAtom.class));
        assertThat("Should divide longs and double", (Double) result.getValue(), is(0.5d));
    }
}
