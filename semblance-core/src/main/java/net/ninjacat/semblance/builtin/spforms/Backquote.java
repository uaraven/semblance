package net.ninjacat.semblance.builtin.spforms;

import com.google.common.collect.ImmutableList;
import net.ninjacat.semblance.data.Constants.HiddenFunctions;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.errors.runtime.UnboundSymbolException;
import net.ninjacat.semblance.evaluator.Context;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Backquote implementation.
 */
public class Backquote extends SpecialForm {

    private static final long serialVersionUID = -8768337748751585237L;

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

    private static LispCollection expandBQ(final LispCollection sExpr, final Context parameterContext) {
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

    private static void processHidden(final List<LispValue> output, final Context context, final LispCollection itemAsList) {
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

    private static void unwrapList(final List<LispValue> output, final Context parameterContext, final LispValue param) {
        final LispCollection coll = asCollection(parameterContext.evaluate(param));
        for (final LispValue value : coll) {
            output.add(value);
        }
    }

    private static boolean isUnwrap(final LispValue func) {
        return func.equals(HiddenFunctions.UNWRAP);
    }

    private static boolean isHidden(final LispValue func) {
        return isUnwrap(func) || isUnquote(func);
    }

    private static boolean isNonEscapedParameter(final LispValue item) {
        return asSymbol(item).repr().startsWith(",");
    }

    private static Collection<LispValue> expandParameter(final LispValue item, final Context parameterContext) {
        final SymbolAtom actualName = asSymbol(item);
        final Optional<LispValue> value = parameterContext.findSymbol(actualName);
        if (value.isPresent()) {
            return ImmutableList.of(value.get());
        } else {
            throw new UnboundSymbolException(asSymbol(item), getSourceInfo(item));
        }
    }

    private static Collection<LispValue> expandList(final LispValue value) {
        return asCollection(value).stream().collect(Collectors.toList());
    }
}
