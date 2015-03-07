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
public class LongAddTest {

    @SuppressWarnings("unchecked")
    @Test
    public void testAddLongs() throws Exception {
        final NumberAtom n1 = new LongNumberAtom(42);
        final NumberAtom n2 = new LongNumberAtom(42);
        final NumberAtom result = n1.add(n2);

        assertThat("Should add two longs", result, instanceOf(LongNumberAtom.class));
        assertThat("Should add two longs", (Long) result.getValue(), is(84L));
    }


    @SuppressWarnings("unchecked")
    @Test
    public void testAddHugeLongs() throws Exception {
        final NumberAtom n1 = new LongNumberAtom(Long.MAX_VALUE);
        final NumberAtom n2 = new LongNumberAtom(Long.MAX_VALUE);
        final NumberAtom result = n1.add(n2);

        assertThat("Should add two longs", result, instanceOf(BigIntegerNumberAtom.class));
        assertThat("Should add two longs", (BigInteger) result.getValue(),
                is(BigInteger.valueOf(Long.MAX_VALUE).multiply(new BigInteger("2"))));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddLongAndBigNoSimplification() throws Exception {
        final NumberAtom n1 = new LongNumberAtom(42);
        final NumberAtom n2 = NumberAtom.make("42000000000000000000000000000");
        final NumberAtom result = n1.add(n2);

        assertThat("Result should be BigInteger", result, instanceOf(BigIntegerNumberAtom.class));
        assertThat("Should add long and Bigint", (BigInteger) result.getValue(), is(new BigInteger("42000000000000000000000000042")));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddLongAndBigWithSimplification() throws Exception {
        final NumberAtom n1 = new LongNumberAtom(42);
        final NumberAtom n2 = new BigIntegerNumberAtom(new BigInteger("42"));
        final NumberAtom result = n1.add(n2);

        assertThat("Result should be BigInteger", result, instanceOf(LongNumberAtom.class));
        assertThat("Should add long and Bigint and convert result to long", (Long) result.getValue(), is(84L));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddLongAndDouble() throws Exception {
        final NumberAtom n1 = new LongNumberAtom(42);
        final NumberAtom n2 = new DoubleNumberAtom(42);
        final NumberAtom result = n1.add(n2);

        assertThat("Result should be double", result, instanceOf(DoubleNumberAtom.class));
        assertThat("Should add longs and double", (Double) result.getValue(), is(84d));
    }
}
