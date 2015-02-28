package net.ninjacat.semblance.data;

import net.ninjacat.semblance.errors.CollectionException;
import net.ninjacat.semblance.java.JavaConvertible;
import org.junit.Test;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class NilCollectionTest {

    @Test
    public void isNilShouldReturnTrue() throws Exception {
        LispCollection collection = getLispCollection();

        assertThat("isNil should be true", collection.isNil(), is(true));
    }

    @Test(expected = CollectionException.class)
    public void headShoudThrowCollectionException() throws Exception {
        LispCollection collection = getLispCollection();

        collection.head();
    }

    @Test(expected = CollectionException.class)
    public void tailShoudThrowCollectionException() throws Exception {
        LispCollection collection = getLispCollection();

        collection.tail();
    }

    @Test
    public void shouldReturnNullAsJavaObject() throws Exception {
        JavaConvertible list = getLispCollection();

        assertThat("Should return null as Java representation", list.asJavaObject(), nullValue());
    }

    @Test
    public void lengthShouldReturnZero() throws Exception {
        LispCollection list = getLispCollection();

        assertThat("Length should be 0", list.length(), is(0L));
    }

    @Test
    public void twoNilCollectionsShouldBeEqual() throws Exception {
        assertThat("Two nil collections should be equal", getLispCollection(), equalTo(getLispCollection()));

    }

    private LispCollection getLispCollection() {
        return new NilCollection();
    }
}
