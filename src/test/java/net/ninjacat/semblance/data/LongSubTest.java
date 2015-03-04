package net.ninjacat.semblance.data;

import org.junit.Test;

import java.math.BigInteger;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

/**
 * Created on 03/03/15.
 */
public class LongSubTest {

    @SuppressWarnings("unchecked")
    @Test
    public void testSubLongs() throws Exception {
        NumberAtom n1 = new LongNumberAtom(42);
        NumberAtom n2 = new LongNumberAtom(42);
        NumberAtom result = n1.sub(n2);

        assertThat("Should subtract two longs", result, instanceOf(LongNumberAtom.class));
        assertThat("Should subtract two longs", (Long) result.getValue(), is(0L));
    }


    @SuppressWarnings("unchecked")
    @Test
    public void testSubHugeLongs() throws Exception {
        NumberAtom n1 = new LongNumberAtom(Long.MIN_VALUE);
        NumberAtom n2 = new LongNumberAtom(Long.MAX_VALUE);
        NumberAtom result = n1.sub(n2);

        assertThat("Should subtract two longs", result, instanceOf(BigIntegerNumberAtom.class));
        assertThat("Should subtract two longs", (BigInteger) result.getValue(),
                is(BigInteger.valueOf(Long.MIN_VALUE).subtract(BigInteger.valueOf(Long.MAX_VALUE))));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testSubLongAndBigNoSimplification() throws Exception {
        NumberAtom n1 = new LongNumberAtom(42);
        NumberAtom n2 = BigIntegerNumberAtom.make("42000000000000000000000000042");
        NumberAtom result = n1.sub(n2);

        assertThat("Result should be BigInteger", result, instanceOf(BigIntegerNumberAtom.class));
        assertThat("Should sutract long and Bigint", (BigInteger) result.getValue(), is(new BigInteger("-42000000000000000000000000000")));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testSubLongAndBigWithSimplification() throws Exception {
        NumberAtom n1 = new LongNumberAtom(42);
        NumberAtom n2 = new BigIntegerNumberAtom(new BigInteger("42"));
        NumberAtom result = n1.sub(n2);

        assertThat("Result should be Long", result, instanceOf(LongNumberAtom.class));
        assertThat("Should subtract long and Bigint and convert result to long", (Long) result.getValue(), is(0L));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testSubLongAndDouble() throws Exception {
        NumberAtom n1 = new LongNumberAtom(0);
        NumberAtom n2 = new DoubleNumberAtom(42);
        NumberAtom result = n1.sub(n2);

        assertThat("Result should be double", result, instanceOf(DoubleNumberAtom.class));
        assertThat("Should subtract longs and double", (Double) result.getValue(), is(-42d));
    }
}
