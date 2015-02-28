package net.ninjacat.semblance.data;

import org.hamcrest.Matchers;
import org.junit.Test;

import java.math.BigInteger;

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


    private Atom getAtom() {
        return new BigIntegerNumberAtom(TEST_NUMBER);
    }


}
