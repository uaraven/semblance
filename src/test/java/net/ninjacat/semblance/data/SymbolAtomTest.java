package net.ninjacat.semblance.data;

import net.ninjacat.semblance.java.Symbol;
import org.junit.Test;

import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.sameInstance;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class SymbolAtomTest {

    public static final String TEST_SYMBOL = "test-symbol";

    @Test
    public void reprShouldReturnCorrectSymbolRepresentation() throws Exception {
        Atom symbol = getAtom();

        assertThat("Should return correct symbol name", symbol.repr(), is(TEST_SYMBOL));
    }

    @Test
    public void shouldConvertToJavaSymbolObject() throws Exception {
        Atom symbol = getAtom();

        Object javaSymbol = symbol.asJavaObject();

        assertThat("Should be represented as Java Symbol object", javaSymbol, instanceOf(Symbol.class));
        assertThat("Java Symbol value should be correct", ((Symbol) javaSymbol).getValue(), is(TEST_SYMBOL));
    }

    @Test
    public void typeNameShouldBeSymbol() throws Exception {
        Atom symbol = getAtom();

        assertThat("Type name should be SYMBOL", symbol.getType(), is(SemblanceType.SYMBOL));
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
        return new SymbolAtom(TEST_SYMBOL);
    }
}
