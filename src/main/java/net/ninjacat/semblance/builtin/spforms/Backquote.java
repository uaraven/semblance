package net.ninjacat.semblance.builtin.spforms;

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

public class Backquote extends SpecialForm {

    public Backquote() {
        super("backquote", "&rest", "body");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        return expandBQ(parameters, context);
    }

    private LispCollection expandBQ(final LispCollection sExpr, final Context parameterContext) {
        final List<LispValue> output = new ArrayList<>();

        for (final LispValue item : sExpr) {
            if (isCollection(item)) {
                output.add(expandBQ(asCollection(item), parameterContext));
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

    private boolean isNonEscapedParameter(final LispValue item) {
        return asSymbol(item).repr().startsWith(",");
    }

    private Collection<LispValue> expandParameter(final LispValue item, final Context parameterContext) {
        final String paramName = item.repr().substring(1);
        final boolean isExpandable = paramName.startsWith("@");
        final SymbolAtom actualName = new SymbolAtom(isExpandable ? paramName.substring(1) : paramName);
        final Option<LispValue> value = parameterContext.findSymbol(actualName);
        if (value.isPresent()) {
            if (isExpandable) {
                return expandList(value.get());
            } else {
                return Lists.of(value.get());
            }
        } else {
            throw new UnboundSymbolException(asSymbol(item), getSourceInfo(item));
        }
    }

    private Collection<LispValue> expandList(final LispValue value) {
        return Iter.of(asCollection(value).iterator()).collectWith(Collectors.<LispValue>arrayList());
    }
}
