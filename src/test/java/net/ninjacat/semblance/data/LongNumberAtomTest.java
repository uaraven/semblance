package net.ninjacat.semblance.data;

import org.hamcrest.Matchers;
import org.junit.Test;

import java.math.BigInteger;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class LongNumberAtomTest {

    public static final long TEST_NUMBER = 123321;

    @Test
    public void reprShouldReturnCorrectStringRepresentation() throws Exception {
        Atom symbol = getAtom();

        assertThat("Should return correct symbol name", symbol.repr(), is(String.valueOf(TEST_NUMBER)));
    }

    @Test
    public void shouldConvertToJavaStringObject() throws Exception {
        Atom symbol = getAtom();

        Object javaSymbol = symbol.asJavaObject();

        assertThat("Should be represented as Java Long object", javaSymbol, Matchers.instanceOf(Long.class));
        assertThat("Java Symbol value should be correct", (Long) javaSymbol, is(TEST_NUMBER));
    }

    @Test
    public void typeNameShouldBeSymbol() throws Exception {
        Atom symbol = getAtom();

        assertThat("Type name should be STRING", symbol.getType(), is(SemblanceType.INTEGER));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddLongs() throws Exception {
        NumberAtom n1 = new LongNumberAtom(42);
        NumberAtom n2 = new LongNumberAtom(42);
        NumberAtom result = n1.add(n2);

        assertThat("Should add two longs", result, instanceOf(LongNumberAtom.class));
        assertThat("Should add two longs", (Long) result.getValue(), is(84L));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddLongAndBigNoSimplification() throws Exception {
        NumberAtom n1 = new LongNumberAtom(42);
        NumberAtom n2 = BigIntegerNumberAtom.make("42000000000000000000000000000");
        NumberAtom result = n1.add(n2);

        assertThat("Result should be BigInteger", result, instanceOf(BigIntegerNumberAtom.class));
        assertThat("Should add long and Bigint", (BigInteger) result.getValue(), is(new BigInteger("42000000000000000000000000042")));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddLongAndBigWithSimplification() throws Exception {
        NumberAtom n1 = new LongNumberAtom(42);
        NumberAtom n2 = new BigIntegerNumberAtom(new BigInteger("42"));
        NumberAtom result = n1.add(n2);

        assertThat("Result should be BigInteger", result, instanceOf(LongNumberAtom.class));
        assertThat("Should add long and Bigint and convert result to long", (Long) result.getValue(), is(84L));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddLongAndDouble() throws Exception {
        NumberAtom n1 = new LongNumberAtom(42);
        NumberAtom n2 = new DoubleNumberAtom(42);
        NumberAtom result = n1.add(n2);

        assertThat("Result should be double", result, instanceOf(DoubleNumberAtom.class));
        assertThat("Should add longs and double", (Double) result.getValue(), is(84d));
    }

    private Atom getAtom() {
        return new LongNumberAtom(TEST_NUMBER);
    }
}

