package net.ninjacat.semblance.data;

import org.junit.Test;

import static net.ninjacat.semblance.data.SemblanceNumberType.BIG;
import static net.ninjacat.semblance.data.SemblanceNumberType.LONG;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

@SuppressWarnings({"NonBooleanMethodNameMayNotStartWithQuestion", "DuplicateStringLiteralInspection"})
public class NumberAtomTest {

    @Test
    public void shouldCreateLongRepresentation() throws Exception {
        final NumberAtom<?> atom = NumberAtom.make("1234");

        assertThat("number should be of LONG type", atom.getNumberType(), is(LONG));
    }

    @Test
    public void shouldCreateLongRepresentationForNegativeNumber() throws Exception {
        final NumberAtom<?> atom = NumberAtom.make("-1234");

        assertThat("number should be of LONG type", atom.getNumberType(), is(LONG));
    }

    @Test
    public void shouldCreateBigIntRepresentation() throws Exception {
        final NumberAtom<?> atom = NumberAtom.make("123456789012345678901234567890");

        assertThat("number should be of BIG type", atom.getNumberType(), is(BIG));
    }

    @Test
    public void shouldCreateBigIntForNegativeNumber() throws Exception {
        final NumberAtom<?> atom = NumberAtom.make("-123456789012345678901234567890");

        assertThat("number should be of BIG type", atom.getNumberType(), is(BIG));
    }
}
