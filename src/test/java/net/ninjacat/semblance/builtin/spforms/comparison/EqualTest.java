package net.ninjacat.semblance.builtin.spforms.comparison;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.data.collections.Vector;
import net.ninjacat.semblance.evaluator.RootContext;
import org.junit.Before;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

@SuppressWarnings("NonBooleanMethodNameMayNotStartWithQuestion")
public class EqualTest {

    private RootContext rootContext;

    @Before
    public void setUp() throws Exception {
        rootContext = new RootContext();
    }

    @Test
    public void shouldCompareTwoNumbersToTrue() throws Exception {
        final SList params = list(number(42), number(42));

        final LispValue result = new Equal().apply(rootContext, params);

        assertThat(asSymbol(result), is(Constants.TRUE));
    }

    @Test
    public void shouldCompareSeveralNumbersToTrue() throws Exception {
        final SList params = list(number(42), number(42), number("42"), number(42.0));

        final LispValue result = new Equal().apply(rootContext, params);

        assertThat(asSymbol(result), is(Constants.TRUE));
    }

    @Test
    public void shouldReturnFalseForDifferentNumbers() throws Exception {
        final SList params = list(number(42), number(2));

        final LispValue result = new Equal().apply(rootContext, params);

        assertThat(asSymbol(result), is(Constants.FALSE));
    }

    @Test
    public void shouldReturnFalseIfAnyInTheSetDiffers() throws Exception {
        final SList params = list(number(42), number(42), number("42"), number(42.1));

        final LispValue result = new Equal().apply(rootContext, params);

        assertThat(asSymbol(result), is(Constants.FALSE));
    }

    @Test
    public void shouldReturnFalseWhenComparingDifferentType() throws Exception {
        final SList params = list(number(42), string("42"));

        final LispValue result = new Equal().apply(rootContext, params);

        assertThat(asSymbol(result), is(Constants.FALSE));
    }

    @Test
    public void shouldReturnTrueForEqualStrings() throws Exception {
        final SList params = list(string("42"), string("42"));

        final LispValue result = new Equal().apply(rootContext, params);

        assertThat(asSymbol(result), is(Constants.TRUE));
    }

    @Test
    public void shouldReturnFalseForStringAndSymbol() throws Exception {
        final SList params = list(string("42"), symbol("42"));

        final LispValue result = new Equal().apply(rootContext, params);

        assertThat(asSymbol(result), is(Constants.FALSE));
    }

    @Test
    public void shouldReturnTrueForEqualVectors() throws Exception {
        final Vector vector = vector(number(1), number(2), number(3));
        final SList params = list(vector, vector);

        final LispValue result = new Equal().apply(rootContext, params);

        assertThat(asSymbol(result), is(Constants.TRUE));
    }

    @Test
    public void shouldReturnFalseForDifferentVectors() throws Exception {
        final Vector vector = vector(number(1), number(2), number(3));
        final SList params = list(vector, vector(number(1), number(2), number(4)));

        final LispValue result = new Equal().apply(rootContext, params);

        assertThat(asSymbol(result), is(Constants.FALSE));
    }

}