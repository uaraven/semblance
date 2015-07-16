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

    @Test
    public void equalShouldReturnTrueOnEqualStrings() throws Exception {
        final Atom atom1 = new StringAtom("This is string atom!");
        final Atom atom2 = new StringAtom("This is string atom!");

        assertThat("Equals should return true", atom1.equals(atom2), is(true));
    }

    @Test
    public void equalShouldReturnFalseOnDifferentStrings() throws Exception {
        final Atom atom1 = new StringAtom("This is not a string atom!");
        final Atom atom2 = new StringAtom("This is string atom!");

        assertThat("Equals should return false", atom1.equals(atom2), is(false));
    }

    private Atom getAtom() {
        return new StringAtom(TEST_STRING);
    }
}
