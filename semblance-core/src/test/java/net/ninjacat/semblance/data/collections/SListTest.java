package net.ninjacat.semblance.data.collections;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SemblanceType;
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

@SuppressWarnings({"NonBooleanMethodNameMayNotStartWithQuestion", "DuplicateStringLiteralInspection"})
public class SListTest {

    private Context context;

    @Before
    public void setUp() throws Exception {
        context = mock(Context.class);
    }

    @Test
    public void shouldReturnCorrectLength() throws Exception {
        final SList list = smartList(1L, 2L, 3L);

        assertThat("length of list should be equal to 3", list.length(), is(3));
    }

    @Test
    public void lengthShouldBeZeroWhenListIsEmpty() throws Exception {
        final LispCollection list = smartList();

        assertThat("length of list should be equal to 0", list.length(), is(0));
    }

    @Test
    public void isNilShouldBeFalseWhenListIsNotEmpty() throws Exception {
        final SList list = smartList(1L, 2L, 3L);

        assertThat("isNil() should be false", list.isNil(), is(false));
    }

    @Test
    public void isNilShouldBeTrueWhenListIsEmpty() throws Exception {
        final SList list = smartList();

        assertThat("isNil() should be false", list.isNil(), is(true));
    }

    @Test
    public void headShouldReturnTheFirstElement() throws Exception {
        final SList list = smartList(1L, 2L, 3L);

        final LispValue head = list.head();

        assertThat("head should return the first element", head, is(number(1L)));
    }

    @Test
    public void tailShouldReturnAllButTheFirstElement() throws Exception {
        final SList list = smartList(1L, 2L, 3L);

        final LispCollection tail = list.tail();

        assertThat("tail should return all but the first element", tail, equalTo((LispCollection) smartList(2L, 3L)));
    }

    @Test(expected = CollectionException.class)
    public void headShouldFailOnEmptyVector() throws Exception {
        smartList().head();
    }

    @Test(expected = CollectionException.class)
    public void tailShouldFailOnEmptyVector() throws Exception {
        smartList().tail();
    }

    @Test
    public void tailShouldReturnNilOnOneElementVector() throws Exception {
        final LispCollection tail = smartList(1L).tail();

        assertThat("tail should return nil on 1-element list", tail.isNil(), is(true));
    }

    @Test
    public void listTypeShouldBeList() throws Exception {
        final SemblanceType type = smartList(1L).getType();

        assertThat("list type should be LIST", type, is(SemblanceType.LIST));
    }

    @Test
    public void listShouldConvertToJavaList() throws Exception {
        final SList slist = smartList(1L, 2L);

        final List<?> list = slist.asJavaObject();

        assertThat("Number of elements in Java list should match number of elements in slist", list.size(), is(slist.length()));
        assertThat("First element in Java list should match same element in slist",
                atom(list.get(0)), is(slist.get(0)));
        assertThat("Second element in Java list should match same element in slist",
                atom(list.get(1)), is(slist.get(1)));
    }

    @Test
    public void shouldGenerateCorrectRepr() throws Exception {
        final SList list = smartList(1L, 2L);

        assertThat("repr() should return correct string representation", list.repr(), is("(1 2)"));
    }

    @Test
    public void listAsFunctionShouldReturnValueByIndex() throws Exception {
        final SList list = smartList(1L, 2L);
        when(context.evaluate(number(1))).thenReturn(number(1));

        final LispValue value = list.apply(context, smartList(1L));

        assertThat("list.apply should return value by index", value, is(atom(2L)));
    }

    @Test
    public void listAsFunctionShouldReturnValueByNegativeIndex() throws Exception {
        final SList list = smartList(1L, 2L, 3L, 4L);
        when(context.evaluate(number(-2))).thenReturn(number(-2));

        final LispValue value = list.apply(context, smartList(-2L));

        assertThat("list.apply should return value by index", value, is(atom(3L)));
    }

    @Test(expected = CollectionIndexOutOfBoundsException.class)
    public void listAsFunctionShouldFailWhenIndexIsOutOfBounds() throws Exception {
        final SList list = smartList(1L, 2L);
        when(context.evaluate(number(3))).thenReturn(number(3));

        list.apply(context, smartList(3L));
    }

    @Test(expected = ValueExpectedException.class)
    public void listAsFunctionShouldFailWhenNoParameters() throws Exception {
        final SList list = smartList(1L, 2L);

        list.apply(context, new NilCollection());
    }

    @Test
    public void similarListsShouldBeEqual() throws Exception {
        final SList list1 = smartList(1L, 2L);
        final SList list2 = smartList(1L, 2L);

        assertThat("Similar lists should be equal", list1, equalTo(list2));
    }

    @Test
    public void differentSizeListsShouldNotBeEqual() throws Exception {
        final SList list1 = smartList(1L, 2L);
        final SList list2 = smartList(1L, 2L, 3L);

        assertThat("Different size lists should not be equal", list1, not(equalTo(list2)));
    }

    @Test
    public void differentValueListShouldNotBeEqual() throws Exception {
        final SList list1 = smartList(1L, 2L);
        final SList list2 = smartList(1L, 3L);

        assertThat("Different value lists should not be equal", list1, not(equalTo(list2)));
    }

    @Test
    public void nonEmptyListShouldNotBeEqualtoNil() throws Exception {
        final LispCollection list1 = smartList(1L, 2L);
        final LispCollection list2 = new NilCollection();

        assertThat("Non-empty list should not be equal to NIL", list2, not(equalTo(list1)));
    }

    @Test
    public void emptyListShouldBeEqualtoNil() throws Exception {
        final LispCollection list1 = smartList();
        final LispCollection list2 = new NilCollection();

        assertThat("Empty list should be equal to NIL", list2, equalTo(list1));
    }

}
