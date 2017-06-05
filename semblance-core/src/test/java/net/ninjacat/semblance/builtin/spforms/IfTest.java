package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.RootContext;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

@SuppressWarnings({"NonBooleanMethodNameMayNotStartWithQuestion", "DuplicateStringLiteralInspection"})
public class IfTest {

    private static final SymbolAtom QUOTE = symbol("quote");
    private static final SymbolAtom SYMBOL_A = symbol(":A");
    private static final SymbolAtom SYMBOL_B = symbol(":B");
    private static final SList COND_TRUE = list(QUOTE, symbol("T"));
    private static final SList COND_FALSE = list(QUOTE, symbol("F"));

    @Test
    public void shouldReturnValueOfTrueBranch() throws Exception {
        final SList program = list(
                symbol("if"), COND_TRUE,
                list(QUOTE, SYMBOL_A)
        );
        final Context rootContext = new RootContext();

        final LispValue result = rootContext.evaluate(program);

        assertThat((SymbolAtom) result, is(SYMBOL_A));
    }

    @Test
    public void shouldReturnNil() throws Exception {
        final SList program = list(
                symbol("if"), NilCollection.INSTANCE,
                list(QUOTE, SYMBOL_A)
        );
        final Context rootContext = new RootContext();

        final LispValue result = rootContext.evaluate(program);

        assertThat((NilCollection) result, is(NilCollection.INSTANCE));
    }


    @Test
    public void shouldReturnValueOfFalseBranch() throws Exception {
        final SList program = list(
                symbol("if"), COND_FALSE,
                list(QUOTE, SYMBOL_A),
                list(QUOTE, SYMBOL_B)
        );
        final Context rootContext = new RootContext();

        final LispValue result = rootContext.evaluate(program);

        assertThat((SymbolAtom) result, is(SYMBOL_B));
    }


    @Test
    public void shouldReturnNilWhenConditionIsNil() throws Exception {
        final SList program = list(
                symbol("if"), COND_FALSE,
                list(QUOTE, SYMBOL_A)
        );
        final Context rootContext = new RootContext();

        final LispValue result = rootContext.evaluate(program);

        assertThat((NilCollection) result, is(NilCollection.INSTANCE));
    }

    @Test
    public void shouldReturnValueOfFalseBranchWhenConditionIsNil() throws Exception {
        final SList program = list(
                symbol("if"), NilCollection.INSTANCE,
                list(QUOTE, SYMBOL_A),
                list(QUOTE, SYMBOL_B)
        );
        final Context rootContext = new RootContext();

        final LispValue result = rootContext.evaluate(program);

        assertThat((SymbolAtom) result, is(SYMBOL_B));
    }

    @Test
    public void shouldEvaluateNumberAsTrue() throws Exception {
        final SList program = list(
                symbol("if"), number(1),
                list(QUOTE, SYMBOL_A),
                list(QUOTE, SYMBOL_B)
        );
        final Context rootContext = new RootContext();

        final LispValue result = rootContext.evaluate(program);

        assertThat((SymbolAtom) result, is(SYMBOL_A));
    }

    @Test
    public void shouldEvaluateSymbolAsTrue() throws Exception {
        final SList program = list(
                symbol("if"), symbol(":symbol"),
                list(QUOTE, SYMBOL_A),
                list(QUOTE, SYMBOL_B)
        );
        final Context rootContext = new RootContext();

        final LispValue result = rootContext.evaluate(program);

        assertThat((SymbolAtom) result, is(SYMBOL_A));
    }


    @Test
    public void shouldEvaluateFSymbolAsFalse() throws Exception {
        final SList program = list(
                symbol("if"), Constants.FALSE,
                list(QUOTE, SYMBOL_A),
                list(QUOTE, SYMBOL_B)
        );
        final Context rootContext = new RootContext();

        final LispValue result = rootContext.evaluate(program);

        assertThat((SymbolAtom) result, is(SYMBOL_B));
    }
}