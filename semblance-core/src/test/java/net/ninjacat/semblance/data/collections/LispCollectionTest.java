package net.ninjacat.semblance.data.collections;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.operations.Operation;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.RootContext;
import org.hamcrest.Matchers;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.list;
import static net.ninjacat.semblance.utils.Values.smartList;
import static org.hamcrest.MatcherAssert.assertThat;

public class LispCollectionTest {

    @Test
    public void shouldSortAlphabetically() throws Exception {
        final Context context = new RootContext();
        final LispValue value = smartList("kilo", "zulu", "bravo").apply(context, list(Operation.SORT.asSymbol()));

        assertThat(value, Matchers.<LispValue>is(smartList("bravo", "kilo", "zulu")));
    }

    @Test
    public void shouldSortDescendingWithFunction() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("('(4 6 1 2) :sortf (fn (x y) (if (>= x y) -1 1)))");

        assertThat(value, Matchers.<LispValue>is(smartList(6, 4, 2, 1)));
    }

    @Test
    public void shouldAppendValues() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("('(\"a\" \"b\" \"c\") :append 1 2)");

        assertThat(value, Matchers.<LispValue>is(smartList("a", "b", "c", 1, 2)));

    }

    @Test
    public void shouldUnwrapAndAppendList() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run("('(\"a\" \"b\" \"c\") :append '(1 2))");

        assertThat(value, Matchers.<LispValue>is(smartList("a", "b", "c", 1, 2)));
    }

}