package net.ninjacat.semblance.data;

import org.hamcrest.Matchers;
import org.junit.Test;

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

    private Atom getAtom() {
        return new DoubleNumberAtom(TEST_NUMBER);
    }


}
