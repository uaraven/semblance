package net.ninjacat.semblance.data;

import org.junit.Test;

import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

@SuppressWarnings({"DuplicateStringLiteralInspection", "NonBooleanMethodNameMayNotStartWithQuestion"})
public class StringAtomTest {

    public static final String TEST_STRING = "test string";

    @Test
    public void reprShouldReturnCorrectStringRepresentation() throws Exception {
        final Atom symbol = getAtom();

        assertThat("Should return correct symbol name", symbol.repr(), is("\"" + TEST_STRING + "\""));
    }

    @Test
    public void shouldConvertToJavaStringObject() throws Exception {
        final Atom symbol = getAtom();

        final Object javaSymbol = symbol.asJavaObject();

        assertThat("Should be represented as Java String object", javaSymbol, instanceOf(String.class));
        assertThat("Java Symbol value should be correct", javaSymbol.toString(), is(TEST_STRING));
    }

    @Test
    public void typeNameShouldBeSymbol() throws Exception {
        final Atom symbol = getAtom();

        assertThat("Type name should be STRING", symbol.getType(), is(SemblanceType.STRING));
    }

    private Atom getAtom() {
        return new StringAtom(TEST_STRING);
    }
}
