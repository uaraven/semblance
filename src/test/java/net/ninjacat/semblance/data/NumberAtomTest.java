package net.ninjacat.semblance.data;

import org.junit.Test;

import static net.ninjacat.semblance.data.SemblanceNumberType.*;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class NumberAtomTest {

    @Test
    public void shouldCreateLongRepresentation() throws Exception {
        NumberAtom<?> atom = NumberAtom.make("1234");

        assertThat("number should be of LONG type", atom.getNumberType(), is(LONG));
    }

    @Test
    public void shouldCreateLongRepresentationForNegativeNumber() throws Exception {
        NumberAtom<?> atom = NumberAtom.make("-1234");

        assertThat("number should be of LONG type", atom.getNumberType(), is(LONG));
    }

    @Test
    public void shouldCreateDoubleRepresentation() throws Exception {
        NumberAtom<?> atom = NumberAtom.make("1234.1");

        assertThat("number should be of DOUBLE type", atom.getNumberType(), is(DOUBLE));
    }

    @Test
    public void shouldCreateDuobleRepresentationForNegativeNumber() throws Exception {
        NumberAtom<?> atom = NumberAtom.make("-1234.2");

        assertThat("number should be of DOUBLE type", atom.getNumberType(), is(DOUBLE));
    }

    @Test
    public void shouldCreateDoubleRepresentationForScientificNumber() throws Exception {
        NumberAtom<?> atom = NumberAtom.make("10e12");

        assertThat("number should be of DOUBLE type", atom.getNumberType(), is(DOUBLE));
    }

    @Test
    public void shouldCreateDuobleRepresentationForNegativeScientificNumber() throws Exception {
        NumberAtom<?> atom = NumberAtom.make("-10e12");

        assertThat("number should be of DOUBLE type", atom.getNumberType(), is(DOUBLE));
    }

    @Test
    public void shouldCreateBigIntRepresentation() throws Exception {
        NumberAtom<?> atom = NumberAtom.make("123456789012345678901234567890");

        assertThat("number should be of BIG type", atom.getNumberType(), is(BIG));
    }

    @Test
    public void shouldCreateBigIntRepresentationForNegativeNumber() throws Exception {
        NumberAtom<?> atom = NumberAtom.make("-123456789012345678901234567890");

        assertThat("number should be of BIG type", atom.getNumberType(), is(BIG));
    }
}
