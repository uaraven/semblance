package net.ninjacat.semblance.data;

import org.hamcrest.Matchers;
import org.junit.Test;

import java.math.BigInteger;

import static net.ninjacat.semblance.utils.Values.number;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

@SuppressWarnings({"NonBooleanMethodNameMayNotStartWithQuestion", "DuplicateStringLiteralInspection", "SpellCheckingInspection"})
public class BigIntegerNumberAtomTest {

    private static final BigInteger TEST_NUMBER = new BigInteger("12332125234234213432143214532654325432543");

    @Test
    public void reprShouldReturnCorrectStringRepresentation() throws Exception {
        final Atom symbol = getAtom();

        assertThat("Should return correct symbol name", symbol.repr(), is(String.valueOf(TEST_NUMBER)));
    }

    @Test
    public void shouldConvertToJavaStringObject() throws Exception {
        final Atom symbol = getAtom();

        final Object javaSymbol = symbol.asJavaObject();

        assertThat("Should be represented as Java Double object", javaSymbol, Matchers.instanceOf(BigInteger.class));
        assertThat("Java Symbol value should be correct", (BigInteger) javaSymbol, is(TEST_NUMBER));
    }

    @Test
    public void typeNameShouldBeSymbol() throws Exception {
        final Atom symbol = getAtom();

        assertThat("Type name should be STRING", symbol.getType(), is(SemblanceType.INTEGER));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddBigInts() throws Exception {
        final NumberAtom n1 = new BigIntegerNumberAtom(new BigInteger("42"));
        final NumberAtom n2 = new BigIntegerNumberAtom(new BigInteger("42"));
        final NumberAtom result = n1.add(n2);

        assertThat("Result should be long", result, instanceOf(LongNumberAtom.class));
        assertThat("Should add two bigints", (Long) result.getValue(), is(84L));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddBigsNoSimplification() throws Exception {
        final NumberAtom n1 = new BigIntegerNumberAtom(new BigInteger("42000000000000000000"));
        final NumberAtom n2 = new BigIntegerNumberAtom(new BigInteger("42000000000000000000"));
        final NumberAtom result = n1.add(n2);

        assertThat("Result should be long", result, instanceOf(BigIntegerNumberAtom.class));
        assertThat("Should add two bigints", (BigInteger) result.getValue(), is(new BigInteger("84000000000000000000")));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddBigAndDouble() throws Exception {
        final NumberAtom n1 = new BigIntegerNumberAtom(new BigInteger("42"));
        final NumberAtom n2 = new DoubleNumberAtom(42);
        final NumberAtom result = n1.add(n2);

        assertThat("Result should be Double", result, instanceOf(DoubleNumberAtom.class));
        assertThat("Should add double and Bigint", (Double) result.getValue(), is(84d));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddBigAndLongWithSimplification() throws Exception {
        final NumberAtom n1 = new BigIntegerNumberAtom(new BigInteger("42"));
        final NumberAtom n2 = new LongNumberAtom(42);
        final NumberAtom result = n1.add(n2);

        assertThat("Result should be long", result, instanceOf(LongNumberAtom.class));
        assertThat("Should add big and long", (Long) result.getValue(), is(84L));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testAddBigAndLongNoSimplification() throws Exception {
        final NumberAtom n1 = new BigIntegerNumberAtom(new BigInteger("42000000000000000000"));
        final NumberAtom n2 = new LongNumberAtom(42);
        final NumberAtom result = n1.add(n2);

        assertThat("Result should be bigint", result, instanceOf(BigIntegerNumberAtom.class));
        assertThat("Should add two bigints", (BigInteger) result.getValue(), is(new BigInteger("42000000000000000042")));
    }

    @Test
    public void shouldMinifyToLong() throws Exception {
        final BigIntegerNumberAtom minLong = new BigIntegerNumberAtom(new BigInteger(String.valueOf(Long.MIN_VALUE)));
        final BigIntegerNumberAtom maxLong = new BigIntegerNumberAtom(new BigInteger(String.valueOf(Long.MAX_VALUE)));
        final BigIntegerNumberAtom normLong = new BigIntegerNumberAtom(new BigInteger("100000"));
        final BigIntegerNumberAtom normNegativeLong = new BigIntegerNumberAtom(new BigInteger("-100000"));

        final BigIntegerNumberAtom positiveBig = new BigIntegerNumberAtom(new BigInteger("9999999999999999999999999"));
        final BigIntegerNumberAtom negativeBig = new BigIntegerNumberAtom(new BigInteger("9999999999999999999999999"));

        assertThat(minLong.minify(), is(new LongNumberAtom(Long.MIN_VALUE)));
        assertThat(maxLong.minify(), is(new LongNumberAtom(Long.MAX_VALUE)));

        assertThat(normLong.minify(), is(number(100000)));
        assertThat(normNegativeLong.minify(), is(number(-100000)));

        assertThat(positiveBig.minify(), Matchers.instanceOf(BigIntegerNumberAtom.class));
        assertThat(negativeBig.minify(), Matchers.instanceOf(BigIntegerNumberAtom.class));
    }

    private Atom getAtom() {
        return new BigIntegerNumberAtom(TEST_NUMBER);
    }


}
