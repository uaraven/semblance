package net.ninjacat.semblance.data;

import org.hamcrest.Matchers;
import org.junit.Test;

import java.math.BigInteger;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class BigIntegerNumberAtomTest {

    public static final BigInteger TEST_NUMBER = new BigInteger("12332125234234213432143214532654325432543");

    @Test
    public void reprShouldReturnCorrectStringRepresentation() throws Exception {
        Atom symbol = getAtom();

        assertThat("Should return correct symbol name", symbol.repr(), is(String.valueOf(TEST_NUMBER)));
    }

    @Test
    public void shouldConvertToJavaStringObject() throws Exception {
        Atom symbol = getAtom();

        Object javaSymbol = symbol.asJavaObject();

        assertThat("Should be represented as Java Double object", javaSymbol, Matchers.instanceOf(BigInteger.class));
        assertThat("Java Symbol value should be correct", (BigInteger) javaSymbol, is(TEST_NUMBER));
    }

    @Test
    public void typeNameShouldBeSymbol() throws Exception {
        Atom symbol = getAtom();

        assertThat("Type name should be STRING", symbol.getType(), is(SemblanceType.INTEGER));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddBigs() throws Exception {
        NumberAtom n1 = new BigIntegerNumberAtom(new BigInteger("42"));
        NumberAtom n2 = new BigIntegerNumberAtom(new BigInteger("42"));
        NumberAtom result = n1.add(n2);

        assertThat("Result should be long", result, instanceOf(LongNumberAtom.class));
        assertThat("Should add two bigints", (Long) result.getValue(), is(84L));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddBigsNoSimplification() throws Exception {
        NumberAtom n1 = new BigIntegerNumberAtom(new BigInteger("42000000000000000000"));
        NumberAtom n2 = new BigIntegerNumberAtom(new BigInteger("42000000000000000000"));
        NumberAtom result = n1.add(n2);

        assertThat("Result should be long", result, instanceOf(BigIntegerNumberAtom.class));
        assertThat("Should add two bigints", (BigInteger) result.getValue(), is(new BigInteger("84000000000000000000")));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddBigAndDouble() throws Exception {
        NumberAtom n1 = new BigIntegerNumberAtom(new BigInteger("42"));
        NumberAtom n2 = new DoubleNumberAtom(42);
        NumberAtom result = n1.add(n2);

        assertThat("Result should be Double", result, instanceOf(DoubleNumberAtom.class));
        assertThat("Should add double and Bigint", (Double) result.getValue(), is(84d));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddBigAndLongWithSimplification() throws Exception {
        NumberAtom n1 = new BigIntegerNumberAtom(new BigInteger("42"));
        NumberAtom n2 = new LongNumberAtom(42);
        NumberAtom result = n1.add(n2);

        assertThat("Result should be long", result, instanceOf(LongNumberAtom.class));
        assertThat("Should add big and long", (Long) result.getValue(), is(84L));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddBigAndLongNoSimplification() throws Exception {
        NumberAtom n1 = new BigIntegerNumberAtom(new BigInteger("42000000000000000000"));
        NumberAtom n2 = new LongNumberAtom(42);
        NumberAtom result = n1.add(n2);

        assertThat("Result should be bigint", result, instanceOf(BigIntegerNumberAtom.class));
        assertThat("Should add two bigints", (BigInteger) result.getValue(), is(new BigInteger("42000000000000000042")));
    }

    private Atom getAtom() {
        return new BigIntegerNumberAtom(TEST_NUMBER);
    }


}
