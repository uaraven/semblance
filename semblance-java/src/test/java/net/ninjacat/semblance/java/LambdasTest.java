package net.ninjacat.semblance.java;


import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import org.junit.Test;

import static net.ninjacat.semblance.java.Lambdas.methodAsFunction;
import static net.ninjacat.semblance.java.Lambdas.methodAsSForm;
import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class LambdasTest {

    @Test
    public void testShouldDoSimpleMethodCall() throws Exception {
        final Interpreter interpreter = new Interpreter();
        interpreter.getRootContext().bind(symbol("concat"), methodAsFunction(this::concat));

        final LispValue value = interpreter.run("(concat \"abc\" \"def\")");

        assertThat(value, is(string("abcdef")));
    }

    @Test
    public void testShouldDoSimpleMethodCallAsSForm() throws Exception {
        final Interpreter interpreter = new Interpreter();
        interpreter.getRootContext().bind(symbol("concat"), methodAsSForm(this::concat2));

        final LispValue value = interpreter.run("(set1 x \"abc\") (set1 y \"def\") (concat x y)");

        assertThat(value, is(string("abcdef")));
    }

    public LispValue concat(final Context context, final LispCollection params) {
        final String first = asString(params.head()).getValue();
        final String second = asString(params.tail().head()).getValue();

        return string(first + second);
    }

    public LispValue concat2(final Context context, final LispCollection params) {
        final LispCollection eval = context.evaluateList(params);
        final String first = asString(eval.head()).getValue();
        final String second = asString(eval.tail().head()).getValue();

        return string(first + second);
    }
}