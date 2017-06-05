package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.NilCollection;
import org.hamcrest.Matchers;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.symbol;
import static org.junit.Assert.assertThat;

public class CondTest {

    @Test
    public void shouldReturnValueForCondition1() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(cond "
                        + "(T :cond1)"
                        + "(F :cond2)" +
                        ")");

        assertThat(value, Matchers.<LispValue>is(symbol(":cond1")));
    }

    @Test
    public void shouldReturnValueForCondition2() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(cond "
                        + "(F :cond1)"
                        + "(T :cond2)" +
                        ")");

        assertThat(value, Matchers.<LispValue>is(symbol(":cond2")));
    }

    @Test
    public void shouldReturnCorrectValueForStringComparision() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(set1 x \"a\")"
                        + "(cond "
                        + "  ((= x \"a\") :cond1)"
                        + "  ((= x \"b\") :cond2)" +
                        ")");

        assertThat(value, Matchers.<LispValue>is(symbol(":cond1")));
    }

    @Test
    public void shouldReturnCorrectValueForStringComparision2() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(set1 x \"b\")"
                        + "(cond "
                        + "  ((= x \"a\") :cond1)"
                        + "  ((= x \"b\") :cond2)" +
                        ")");

        assertThat(value, Matchers.<LispValue>is(symbol(":cond2")));
    }

    @Test
    public void shouldReturnCatchAllValueForStringComparision() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(set1 x \"c\")"
                        + "(cond "
                        + "  ((= x \"a\") :cond1)"
                        + "  ((= x \"b\") :cond2)"
                        + "  (T :cond3)"
                        + ")");

        assertThat(value, Matchers.<LispValue>is(symbol(":cond3")));
    }


    @Test
    public void shouldReturnNilValueForStringComparision() throws Exception {
        final Interpreter interpreter = new Interpreter();

        final LispValue value = interpreter.run(
                "(set1 x \"c\")"
                        + "(cond "
                        + "  ((= x \"a\") :cond1)"
                        + "  ((= x \"b\") :cond2)"
                        + ")");

        assertThat(value, Matchers.<LispValue>is(NilCollection.INSTANCE));
    }

}