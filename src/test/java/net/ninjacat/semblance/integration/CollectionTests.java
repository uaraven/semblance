package net.ninjacat.semblance.integration;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.collections.LispValue;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class CollectionTests {

    @Test
    public void testShouldAccessVectorAsFunction() throws Exception {

        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(set x [1 2 3 4])" +
                        "(x 2)");

        assertThat(value, is(number(3)));
    }

    @Test
    public void testShouldAccessListAsFunction() throws Exception {

        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(set x '(1 2 3 4))" +
                        "(x 2)");

        assertThat(value, is(number(3)));
    }


    @Test
    public void testShouldGetListSlice() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(set x '(1 2 3 4))" +
                        "(x 1 2)");

        assertThat(value, is((LispValue) smartList(2L, 3L)));
    }

    @Test
    public void testShouldGetListHeadViaOperator() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(set x '(1 2 3 4))" +
                        "(x :head)");

        assertThat(value, is(number(1)));
    }

    @Test
    public void testShouldGetListTailViaOperator() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(set x '(1 2 3 4))" +
                        "(x :tail)");

        assertThat(value, is((LispValue) smartList(2L, 3L, 4L)));
    }

    @Test
    public void testShouldAccessInPlaceListAsFunction() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "('(1 2 3 4) 2)");

        assertThat(value, is(number(3)));
    }


    @Test
    public void testShouldAccessInPlaceVectorAsFunction() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "([1 2 3 4] 2)");

        assertThat(value, is(number(3)));
    }

    @Test
    public void testShouldAccessInPlaceMapAsFunction() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "({:a 1 :b 2 :c 3} :b)");

        assertThat(value, is(number(2)));
    }

    @Test
    public void testShouldAccessMapAsFunction() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(var (x {:a 1 :b (+ 2 3) :c 3}))" +
                        "(x :b)");

        assertThat(value, is(number(5)));
    }

    @Test
    public void testShouldReturnMapKeys() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(keys {:a 1 :b 2 :c 3})");

        assertThat(asSList(value).getCollection(), containsInAnyOrder((LispValue) symbol(":a"),
                (LispValue) symbol(":b"), (LispValue) symbol(":c")));
    }

    @Test
    public void testShouldTrueIfMapContainsKey() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(contains {:a 1 :b 2 :c 3} :b)");

        assertThat(value, is((LispValue) Constants.TRUE));
    }

    @Test
    public void testShouldFalseIfMapDoesNotContainKey() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(contains {:a 1 :b 2 :c 3} :z)");

        assertThat(value, is((LispValue) Constants.FALSE));
    }

    @Test
    public void testShouldReturnMapValues() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(values {:a 1 :b 2 :c 3})");

        assertThat(asSList(value).getCollection(), containsInAnyOrder(number(1), number(2), number(3)));
    }

    @Test
    public void testShouldUpdateMapAsFunction() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(var (x {:a 1 :b (+ 2 3) :c 3}))" +
                        "(x :b (- 4 2))" +
                        "(x :b)");

        assertThat(value, is(number(2)));
    }

    @Test
    public void testLispOperationHead() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "('(1 2 3 4 5) :head)");

        assertThat(value, is(number(1)));
    }

    @Test
    public void testLispOperationTail() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "('(1 2 3 4 5) :tail)");

        assertThat(value, is((LispValue) smartList(2L, 3L, 4L, 5L)));
    }

    @Test
    public void testLispOperationLast() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "('(1 2 3 4 5) :last)");

        assertThat(value, is(number(5)));
    }

    @Test
    public void testLispOperationTake() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "('(1 2 3 4 5) :take 2)");

        assertThat(value, is((LispValue) smartList(1L, 2L)));
    }

    @Test
    public void testLispOperationDrop() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "('(1 2 3 4 5) :drop 2)");

        assertThat(value, is((LispValue) smartList(3L, 4L, 5L)));
    }

    @Test
    public void testLispOperationReverse() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "('(1 2 3 4 5) :reverse)");

        assertThat(value, is((LispValue) smartList(5L, 4L, 3L, 2L, 1L)));
    }

    @Test
    public void testLispOperationSort() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "('(1 5 4 2 3) :sort)");

        assertThat(value, is((LispValue) smartList(1L, 2L, 3L, 4L, 5L)));
    }

    @Test
    public void testLispOperationSortDesc() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "('(1 5 4 2 3) :sort :desc)");

        assertThat(value, is((LispValue) smartList(5L, 4L, 3L, 2L, 1L)));
    }

    @Test
    public void testShouldTrueIfCollectionContainsKey() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(contains [1 2 3] 2)");

        assertThat(value, is((LispValue) Constants.TRUE));
    }

    @Test
    public void testShouldFalseIfCollectionDoesNotContainKey() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(contains [1 2 3] 5)");

        assertThat(value, is((LispValue) Constants.FALSE));
    }

    @Test
    public void testFindElementIndex() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(find [1 2 3] 3)");

        assertThat(value, is(number(2)));
    }

    @Test
    public void testFindShouldReturnMinusOneIfCannotFindElement() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(find [:a :b :c] :d)");

        assertThat(value, is(number(-1)));
    }


    @Test
    public void testLispOperationSortStrings() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "('(\"bz\" \"ac\" \"ba\") :sort)");

        assertThat(value, is((LispValue) smartList("ac", "ba", "bz")));
    }

    @Test
    public void testLispOperationMapDouble() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(let ((double (fn (x) (* 2 x))))" +
                        "('(1 2 3) :map double))");

        assertThat(value, is((LispValue) smartList(2L, 4L, 6L)));
    }

    @Test
    public void testLispOperationFilterOdd() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(let ((is-odd(fn(x) (!= 0 (% x 2)) )))" +
                        "('(1 2 3 4) :filter is-odd))");

        assertThat(value, is((LispValue) smartList(1L, 3L)));
    }
}
