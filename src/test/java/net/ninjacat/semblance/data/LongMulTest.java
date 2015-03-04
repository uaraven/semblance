package net.ninjacat.semblance.data;

import org.junit.Test;

import java.math.BigInteger;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

/**
 * Created on 03/03/15.
 */
public class LongMulTest {

    @SuppressWarnings("unchecked")
    @Test
    public void testAddLongs() throws Exception {
        NumberAtom n1 = new LongNumberAtom(42);
        NumberAtom n2 = new LongNumberAtom(2);
        NumberAtom result = n1.mul(n2);

        assertThat("Should multiply two longs", result, instanceOf(LongNumberAtom.class));
        assertThat("Should multiply two longs", (Long) result.getValue(), is(84L));
    }


    @SuppressWarnings("unchecked")
    @Test
    public void testMultiplyHugeLongs() throws Exception {
        NumberAtom n1 = new LongNumberAtom(Long.MAX_VALUE);
        NumberAtom n2 = new LongNumberAtom(Long.MAX_VALUE);
        NumberAtom result = n1.mul(n2);

        assertThat("Should mult two longs", result, instanceOf(BigIntegerNumberAtom.class));
        assertThat("Should mult two longs", (BigInteger) result.getValue(),
                is(BigInteger.valueOf(Long.MAX_VALUE).multiply(BigInteger.valueOf(Long.MAX_VALUE))));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testMulLongAndBigNoSimplification() throws Exception {
        NumberAtom n1 = new LongNumberAtom(2);
        NumberAtom n2 = BigIntegerNumberAtom.make("42000000000000000000000000000");
        NumberAtom result = n1.mul(n2);

        assertThat("Result should be BigInteger", result, instanceOf(BigIntegerNumberAtom.class));
        assertThat("Should mult long and Bigint", (BigInteger) result.getValue(), is(new BigInteger("84000000000000000000000000000")));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testMulLongAndBigWithSimplification() throws Exception {
        NumberAtom n1 = new LongNumberAtom(2);
        NumberAtom n2 = new BigIntegerNumberAtom(new BigInteger("42"));
        NumberAtom result = n1.mul(n2);

        assertThat("Result should be BigInteger", result, instanceOf(LongNumberAtom.class));
        assertThat("Should mult long and Bigint and convert result to long", (Long) result.getValue(), is(84L));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testMulLongAndDouble() throws Exception {
        NumberAtom n1 = new LongNumberAtom(42);
        NumberAtom n2 = new DoubleNumberAtom(0.5);
        NumberAtom result = n1.mul(n2);

        assertThat("Result should be double", result, instanceOf(DoubleNumberAtom.class));
        assertThat("Should add longs and double", (Double) result.getValue(), is(21d));
    }
}
