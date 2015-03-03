package net.ninjacat.semblance.data;

import org.hamcrest.Matchers;
import org.junit.Test;

import java.math.BigInteger;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class DoubleNumberAtomTest {

    public static final double TEST_NUMBER = 123321.25;

    @Test
    public void reprShouldReturnCorrectStringRepresentation() throws Exception {
        Atom symbol = getAtom();

        assertThat("Should return correct symbol name", symbol.repr(), is(String.valueOf(TEST_NUMBER)));
    }

    @Test
    public void shouldConvertToJavaStringObject() throws Exception {
        Atom symbol = getAtom();

        Object javaSymbol = symbol.asJavaObject();

        assertThat("Should be represented as Java Double object", javaSymbol, Matchers.instanceOf(Double.class));
        assertThat("Java Symbol value should be correct", (Double) javaSymbol, is(TEST_NUMBER));
    }

    @Test
    public void typeNameShouldBeFloatingPoint() throws Exception {
        Atom symbol = getAtom();

        assertThat("Type name should be FLOATING_POINT", symbol.getType(), is(SemblanceType.FLOATIG_POINT));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddDoubles() throws Exception {
        NumberAtom n1 = new DoubleNumberAtom(42);
        NumberAtom n2 = new DoubleNumberAtom(42);
        NumberAtom result = n1.add(n2);

        assertThat("Result should be double", result, instanceOf(DoubleNumberAtom.class));
        assertThat("Should add two doubles", (Double) result.getValue(), is(84d));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddDoubleAndBig() throws Exception {
        NumberAtom n1 = new DoubleNumberAtom(42);
        NumberAtom n2 = new BigIntegerNumberAtom(new BigInteger("42"));
        NumberAtom result = n1.add(n2);

        assertThat("Result should be Double", result, instanceOf(DoubleNumberAtom.class));
        assertThat("Should add double and Bigint", (Double) result.getValue(), is(84d));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddDoubleAndLong() throws Exception {
        NumberAtom n1 = new DoubleNumberAtom(42);
        NumberAtom n2 = new LongNumberAtom(42);
        NumberAtom result = n1.add(n2);

        assertThat("Result should be double", result, instanceOf(DoubleNumberAtom.class));
        assertThat("Should add longs and double", (Double) result.getValue(), is(84d));
    }

    private Atom getAtom() {
        return new DoubleNumberAtom(TEST_NUMBER);
    }
}
