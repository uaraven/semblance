package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.Constants.HiddenFunctions;
import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.errors.runtime.UnboundSymbolException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.smooth.collections.Lists;
import net.ninjacat.smooth.iterators.Collectors;
import net.ninjacat.smooth.iterators.Iter;
import net.ninjacat.smooth.utils.Option;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Backquote implementation.
 */
public class Backquote extends SpecialForm {

    /**
     * Creates a new instance of BackQuote.
     */
    public Backquote() {
        super("backquote", "&rest", "body");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        return expandBQ(parameters, context);
    }

    private static boolean isUnquote(final LispValue func) {
        return func.equals(HiddenFunctions.COMMA);
    }

    private LispCollection expandBQ(final LispCollection sExpr, final Context parameterContext) {
        final List<LispValue> output = new ArrayList<>();

        for (final LispValue item : sExpr) {
            if (isCollection(item)) {
                final LispCollection itemAsList = asCollection(item);
                final LispValue head = itemAsList.head();
                if (isHidden(head)) {
                    processHidden(output, parameterContext, itemAsList);
                } else {
                    output.add(expandBQ(itemAsList, parameterContext));
                }
            } else {
                if (isSymbol(item) && isNonEscapedParameter(item)) {
                    output.addAll(expandParameter(item, parameterContext));
                } else {
                    output.add(item);
                }
            }
        }

        return new SList(output);
    }

    private void processHidden(final List<LispValue> output, final Context context, final LispCollection itemAsList) {
        final LispValue head = itemAsList.head();
        final LispValue expression = itemAsList.tail().head();
        if (isUnwrap(head)) {
            unwrapList(output, context, expression);
        } else {
            if (isList(expression) && isUnwrap(asCollection(expression).head())) {
                unwrapList(output, context, asCollection(expression).tail().head());
            } else {
                final LispValue unquoted = context.evaluate(expression);
                output.add(unquoted);
            }
        }
    }

    private void unwrapList(final List<LispValue> output, final Context parameterContext, final LispValue param) {
        final LispCollection coll = asCollection(parameterContext.evaluate(param));
        for (final LispValue value : coll) {
            output.add(value);
        }
    }

    private boolean isUnwrap(final LispValue func) {
        return func.equals(HiddenFunctions.UNWRAP);
    }

    private boolean isHidden(final LispValue func) {
        return isUnwrap(func) || isUnquote(func);
    }

    private boolean isNonEscapedParameter(final LispValue item) {
        return asSymbol(item).repr().startsWith(",");
    }

    private Collection<LispValue> expandParameter(final LispValue item, final Context parameterContext) {
        final SymbolAtom actualName = asSymbol(item);
        final Option<LispValue> value = parameterContext.findSymbol(actualName);
        if (value.isPresent()) {
            return Lists.of(value.get());
        } else {
            throw new UnboundSymbolException(asSymbol(item), getSourceInfo(item));
        }
    }

    private Collection<LispValue> expandList(final LispValue value) {
        return Iter.of(asCollection(value).iterator()).collectWith(Collectors.<LispValue>arrayList());
    }
}
