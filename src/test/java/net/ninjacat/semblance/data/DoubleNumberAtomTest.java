package net.ninjacat.semblance.data;

import org.hamcrest.Matchers;
import org.junit.Test;

import static org.hamcrest.Matchers.sameInstance;
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
    public void typeNameShouldBeSymbol() throws Exception {
        Atom symbol = getAtom();

        assertThat("Type name should be STRING", symbol.getType(), is(SemblanceType.NUMBER));
    }

    @Test
    public void selfShouldReturnAtomItself() throws Exception {
        LispValue symbol = getAtom();

        assertThat("Self should return this instance", symbol.self(), sameInstance(symbol));
    }

    @Test
    public void shouldEvaluateToItself() throws Exception {
        LispValue symbol = getAtom();

        assertThat("Should evaluate to self", symbol.evaluate(), sameInstance(symbol));
    }

    private Atom getAtom() {
        return new DoubleNumberAtom(TEST_NUMBER);
    }


}
