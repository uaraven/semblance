package net.ninjacat.semblance.data;

import net.ninjacat.semblance.errors.runtime.CollectionException;
import net.ninjacat.semblance.errors.runtime.CollectionIndexOutOfBoundsException;
import net.ninjacat.semblance.errors.runtime.ValueExpectedException;
import net.ninjacat.semblance.evaluator.Context;
import org.junit.Before;
import org.junit.Test;

import java.util.List;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class VectorTest {

    private Context context;

    @Before
    public void setUp() throws Exception {
        context = mock(Context.class);
    }

    @Test
    public void shouldReturnCorrectLength() throws Exception {
        Vector v = smartVector(1L, 2L, 3L);

        assertThat("length of vector should be equal to 3", v.length(), is(3L));
    }

    @Test
    public void lengthShouldBeZeroWhenVectorIsEmpty() throws Exception {
        Vector v = smartVector();

        assertThat("length of vector should be equal to 0", v.length(), is(0L));
    }

    @Test
    public void isNilShouldBeFalseWhenVectorIsNotEmpty() throws Exception {
        Vector v = smartVector(1L, 2L, 3L);

        assertThat("isNil() should be false", v.isNil(), is(false));
    }

    @Test
    public void isNilShouldBeTrueWhenVectorIsEmpty() throws Exception {
        Vector v = smartVector();

        assertThat("isNil() should be false", v.isNil(), is(true));
    }

    @Test
    public void headShouldReturnTheFirstElement() throws Exception {
        Vector v = smartVector(1L, 2L, 3L);

        LispValue head = v.head();

        assertThat("head should return the first element", head, is(number(1L)));
    }

    @Test
    public void tailShouldReturnAllButTheFirstElement() throws Exception {
        Vector v = smartVector(1L, 2L, 3L);

        LispCollection tail = v.tail();

        assertThat("tail should return all but the first element", tail, equalTo((LispCollection) smartVector(2L, 3L)));
    }

    @Test(expected = CollectionException.class)
    public void headShouldFailOnEmptyVector() throws Exception {
        smartVector().head();
    }

    @Test(expected = CollectionException.class)
    public void tailShouldFailOnEmptyVector() throws Exception {
        smartVector().tail();
    }

    @Test()
    public void tailShouldReturnNilOnOneElementVector() throws Exception {
        LispCollection tail = smartVector(1L).tail();

        assertThat("tail should return nil on 1-element vector", tail.isNil(), is(true));
    }

    @Test
    public void vectorTypeShouldBeVector() throws Exception {
        SemblanceType type = smartVector(1L).getType();

        assertThat("vector type should be VECTOR", type, is(SemblanceType.VECTOR));
    }

    @Test
    public void vectorShouldConvertToJavaList() throws Exception {
        Vector vector = smartVector(1L, 2L);

        List<?> list = vector.asJavaObject();

        assertThat("Number of elements in Java list should match number of elements in vector", (long) list.size(), is(vector.length()));
        assertThat("First element in Java list should match same element in vector",
                atom(list.get(0)), is(vector.get(0)));
        assertThat("Second element in Java list should match same element in vector",
                atom(list.get(1)), is(vector.get(1)));
    }

    @Test
    public void shouldGenerateCorrectRepr() throws Exception {
        Vector vector = smartVector(1L, 2L);

        assertThat("repr() should return correct string representation", vector.repr(), is("[1, 2]"));
    }

    @Test
    public void vectorAsFunctionShouldReturnValueByIndex() throws Exception {
        Vector vector = smartVector(1L, 2L);

        when(context.evaluate(number(1))).thenReturn(number(1));

        LispValue value = vector.apply(context, smartVector(1L));

        assertThat("vector.apply should return value by index", value, is(atom(2L)));
    }

    @Test(expected = CollectionIndexOutOfBoundsException.class)
    public void vectorAsFunctionShouldFailWhenIndexIsOutOfBounds() throws Exception {
        Vector vector = smartVector(1L, 2L);
        when(context.evaluate(number(3))).thenReturn(number(3));

        vector.apply(context, smartVector(3L));
    }

    @Test(expected = ValueExpectedException.class)
    public void vectorAsFunctionShouldFailWhenNoParameters() throws Exception {
        Vector vector = smartVector(1L, 2L);
        when(context.evaluate(NilCollection.INSTANCE)).thenReturn(NilCollection.INSTANCE);

        vector.apply(context, new NilCollection());
    }

    @Test
    public void similarVectorsShouldBeEqual() throws Exception {
        Vector vector1 = smartVector(1L, 2L);
        Vector vector2 = smartVector(1L, 2L);

        assertThat("Similar vectors should be equal", vector1, equalTo(vector2));
    }

    @Test
    public void differentSizeVectorsShouldNotBeEqual() throws Exception {
        Vector vector1 = smartVector(1L, 2L);
        Vector vector2 = smartVector(1L, 2L, 3L);

        assertThat("Different size vectors should not be equal", vector1, not(equalTo(vector2)));
    }

    @Test
    public void differentValueVectorsShouldNotBeEqual() throws Exception {
        Vector vector1 = smartVector(1L, 2L);
        Vector vector2 = smartVector(1L, 3L);

        assertThat("Different value vectors should not be equal", vector1, not(equalTo(vector2)));
    }

    @Test
    public void nonEmptyVectorShouldNotBeEqualtoNil() throws Exception {
        LispCollection vector1 = smartVector(1L, 2L);
        LispCollection vector2 = new NilCollection();

        assertThat("Non-empty vector should not be equal to NIL", vector2, not(equalTo(vector1)));
    }

    @Test
    public void emptyVectorShouldBeEqualtoNil() throws Exception {
        LispCollection vector1 = smartVector();
        LispCollection vector2 = new NilCollection();

        assertThat("Empty vector should be equal to NIL", vector2, equalTo(vector1));
    }
}
