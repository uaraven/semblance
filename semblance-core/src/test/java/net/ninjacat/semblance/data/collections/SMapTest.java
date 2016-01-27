package net.ninjacat.semblance.data.collections;

import org.junit.Test;

import static net.ninjacat.semblance.data.collections.SMap.newSMap;
import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.Matchers.*;
import static org.junit.Assert.assertThat;


@SuppressWarnings("NonBooleanMethodNameMayNotStartWithQuestion")
public class SMapTest {

    @Test
    public void shouldReturnCorrectLength() throws Exception {
        final SMap sMap = newSMap(symbol(":a"), number(1), symbol(":b"), string("2"));
        assertThat("Length should be correct", sMap.length(), is(2));
    }

    @Test
    public void shouldReturnCorrectLengthForEmptyMap() throws Exception {
        final SMap sMap = new SMap();
        assertThat("Length should be correct", sMap.length(), is(0));
    }

    @Test
    public void shouldCompareIdenticalMaps() throws Exception {
        final SMap sMap1 = newSMap(symbol(":a"), number(1), symbol(":b"), string("2"));
        final SMap sMap2 = newSMap(symbol(":a"), number(1), symbol(":b"), string("2"));
        assertThat("Identical maps must be equal", sMap1, equalTo(sMap2));
    }

    @Test
    public void mapsWithDifferentKeysMustNotBeEqual() throws Exception {
        final SMap sMap1 = newSMap(symbol(":a"), number(1), symbol(":b"), string("2"));
        final SMap sMap2 = newSMap(symbol(":a"), number(1), symbol(":c"), string("2"));
        assertThat("Maps with different keys must not be equal", sMap1, not(equalTo(sMap2)));
    }

    @Test
    public void mapsWithDifferentValuesMustNotBeEqual() throws Exception {
        final SMap sMap1 = newSMap(symbol(":a"), number(1), symbol(":b"), string("2"));
        final SMap sMap2 = newSMap(symbol(":a"), number(1), symbol(":b"), number(2));
        assertThat("Maps with different values must not be equal", sMap1, not(equalTo(sMap2)));
    }

    @Test
    public void shouldGenerateCorrectRepresentation() throws Exception {
        final SMap sMap = newSMap(symbol(":a"), number(1), symbol(":b"), string("2"));
        final String repr = sMap.repr();

        assertThat("repr() should generate correct string", repr, equalTo("{:a 1 :b \"2\"}"));
    }
}