package net.ninjacat.semblance.data;

import net.ninjacat.semblance.errors.CollectionException;
import net.ninjacat.semblance.errors.CollectionIndexOutOfBoundsException;
import net.ninjacat.semblance.errors.ValueExpectedException;
import org.junit.Test;

import java.util.List;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.sameInstance;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class SListTest {

    @Test
    public void shouldReturnCorrectLength() throws Exception {
        SList l = smartList(1L, 2L, 3L);

        assertThat("length of list should be equal to 3", l.length(), is(3L));
    }

    @Test
    public void lengthShouldBeZeroWhenListIsEmpty() throws Exception {
        LispCollection l = smartList();

        assertThat("length of list should be equal to 0", l.length(), is(0L));
    }

    @Test
    public void isNilShouldBeFalseWhenListIsNotEmpty() throws Exception {
        SList l = smartList(1L, 2L, 3L);

        assertThat("isNil() should be false", l.isNil(), is(false));
    }

    @Test
    public void isNilShouldBeTrueWhenListIsEmpty() throws Exception {
        SList l = smartList();

        assertThat("isNil() should be false", l.isNil(), is(true));
    }

    @Test
    public void headShouldReturnTheFirstElement() throws Exception {
        SList l = smartList(1L, 2L, 3L);

        LispValue head = l.head();

        assertThat("head should return the first element", head, is(number(1L)));
    }

    @Test
    public void tailShouldReturnAllButTheFirstElement() throws Exception {
        SList l = smartList(1L, 2L, 3L);

        LispCollection tail = l.tail();

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

    @Test()
    public void tailShouldReturnNilOnOneElementVector() throws Exception {
        LispCollection tail = smartList(1L).tail();

        assertThat("tail should return nil on 1-element list", tail.isNil(), is(true));
    }

    @Test
    public void listTypeShouldBeList() throws Exception {
        SemblanceType type = smartList(1L).getType();

        assertThat("list type should be LIST", type, is(SemblanceType.LIST));
    }

    @Test
    public void listShouldConvertToJavaList() throws Exception {
        SList slist = smartList(1L, 2L);

        List<?> list = slist.asJavaObject();

        assertThat("Number of elements in Java list should match number of elements in slist", (long) list.size(), is(slist.length()));
        assertThat("First element in Java list should match same element in slist",
                atom(list.get(0)), is(slist.get(0)));
        assertThat("Second element in Java list should match same element in slist",
                atom(list.get(1)), is(slist.get(1)));
    }


    @Test
    public void shouldEvaluateToItself() throws Exception {
        LispValue list = smartList(1L);

        assertThat("Should evaluate to self", list.evaluate(), sameInstance(list));
    }

    @Test
    public void shouldGenerateCorrectRepr() throws Exception {
        SList list = smartList(1L, 2L);

        assertThat("repr() should return correct string representation", list.repr(), is("(1, 2)"));
    }

    @Test
    public void listAsFunctionShouldReturnValueByIndex() throws Exception {
        SList list = smartList(1L, 2L);

        LispValue value = list.apply(smartList(1L));

        assertThat("list.apply should return value by index", value, is(atom(2L)));
    }

    @Test(expected = CollectionIndexOutOfBoundsException.class)
    public void listAsFunctionShouldFailWhenIndexIsOutOfBounds() throws Exception {
        SList list = smartList(1L, 2L);

        list.apply(smartList(3L));
    }

    @Test(expected = ValueExpectedException.class)
    public void listAsFunctionShouldFailWhenNoParameters() throws Exception {
        SList list = smartList(1L, 2L);

        list.apply(new NilCollection());
    }

    @Test
    public void similarListsShouldBeEqual() throws Exception {
        SList list1 = smartList(1L, 2L);
        SList list2 = smartList(1L, 2L);

        assertThat("Similar lists should be equal", list1, equalTo(list2));
    }

    @Test
    public void differentSizeListsShouldNotBeEqual() throws Exception {
        SList list1 = smartList(1L, 2L);
        SList list2 = smartList(1L, 2L, 3L);

        assertThat("Different size lists should not be equal", list1, not(equalTo(list2)));
    }

    @Test
    public void differentValueListShouldNotBeEqual() throws Exception {
        SList list1 = smartList(1L, 2L);
        SList list2 = smartList(1L, 3L);

        assertThat("Different value lists should not be equal", list1, not(equalTo(list2)));
    }

    @Test
    public void nonEmptyListShouldNotBeEqualtoNil() throws Exception {
        LispCollection list1 = smartList(1L, 2L);
        LispCollection list2 = new NilCollection();

        assertThat("Non-empty list should not be equal to NIL", list2, not(equalTo(list1)));
    }

    @Test
    public void emptyListShouldBeEqualtoNil() throws Exception {
        LispCollection list1 = smartList();
        LispCollection list2 = new NilCollection();

        assertThat("Empty list should be equal to NIL", list2, equalTo(list1));
    }

}
